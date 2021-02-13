use crate::filedb::*;
use alloc::string::{String, ToString};
use alloc::vec;
use alloc::vec::Vec;
use core::fmt::{Error, Result, Write};
use core::ops::Range;

/// Count the number of decimal digits in `n`.
fn count_digits(mut n: usize) -> usize {
    let mut count = 0;
    while n != 0 {
        count += 1;
        n /= 10; // remove last digit
    }
    count
}

/// A label describing an underlined region of code associated with a diagnostic.
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serialization", derive(Serialize, Deserialize))]
pub struct Label {
    /// The file that we are labelling.
    pub file_id: u32,
    /// The range in bytes we are going to include in the final snippet.
    pub range: Range<usize>,
    /// An optional message to provide some additional information for the
    /// underlined code. These should not include line breaks.
    pub message: String,
}

impl Label {
    /// Create a new label.
    pub fn new(file_id: u32, range: impl Into<Range<usize>>) -> Self {
        Label {
            file_id,
            range: range.into(),
            message: String::new(),
        }
    }

    /// Create a new label with a style of [`LabelStyle::Secondary`].
    ///
    /// Add a message to the diagnostic.
    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = message.into();
        self
    }
}

/// Represents a diagnostic message that can provide information like errors and
/// warnings to the user.
///
/// The position of a Diagnostic is considered to be the position of the [`Label`] that has the earliest starting position and has the highest style which appears in all the labels of the diagnostic.
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serialization", derive(Serialize, Deserialize))]
pub struct Diagnostic {
    /// The main message associated with this diagnostic.
    ///
    /// These should not include line breaks, and in order support the 'short'
    /// diagnostic display mod, the message should be specific enough to make
    /// sense on its own, without additional context provided by labels and notes.
    pub message: String,
    /// Source labels that describe the cause of the diagnostic.
    /// The order of the labels inside the vector does not have any meaning.
    /// The labels are always arranged in the order they appear in the source code.
    pub labels: Vec<Label>,
    /// Notes that are associated with the primary cause of the diagnostic.
    /// These can include line breaks for improved formatting.
    pub notes: Vec<String>,
}

impl Diagnostic {
    /// Create a new diagnostic.
    pub fn new() -> Self {
        Diagnostic {
            message: String::new(),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Add a message to the diagnostic.
    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = message.into();
        self
    }

    /// Add some labels to the diagnostic.
    pub fn with_labels(mut self, labels: Vec<Label>) -> Self {
        self.labels = labels;
        self
    }

    /// Add some notes to the diagnostic.
    pub fn with_notes(mut self, notes: Vec<String>) -> Self {
        self.notes = notes;
        self
    }

    pub fn render(&self, files: &FileDb, out: &mut impl Write) -> Result {
        use alloc::collections::BTreeMap;

        struct LabeledFile<'diagnostic, FileId> {
            file_id: FileId,
            start: usize,
            name: String,
            location: Location,
            num_multi_labels: usize,
            lines: BTreeMap<usize, Line<'diagnostic>>,
        }

        impl<'diagnostic, FileId> LabeledFile<'diagnostic, FileId> {
            fn get_or_insert_line(
                &mut self,
                line_index: usize,
                line_range: Range<usize>,
                line_number: usize,
            ) -> &mut Line<'diagnostic> {
                self.lines.entry(line_index).or_insert_with(|| Line {
                    range: line_range,
                    number: line_number,
                    single_labels: vec![],
                    multi_labels: vec![],
                    // This has to be false by default so we know if it must be rendered by another condition already.
                    must_render: false,
                })
            }
        }

        struct Line<'diagnostic> {
            number: usize,
            range: Range<usize>,
            // TODO: How do we reuse these allocations?
            single_labels: Vec<SingleLabel<'diagnostic>>,
            multi_labels: Vec<(usize, MultiLabel<'diagnostic>)>,
            must_render: bool,
        }

        let mut renderer = Renderer::new(out);

        // TODO: Make this data structure external, to allow for allocation reuse
        let mut labeled_files = Vec::<LabeledFile<'_, _>>::new();
        // Keep track of the outer padding to use when rendering the
        // snippets of source code.
        let mut outer_padding = 0;

        // Group labels by file
        for label in &self.labels {
            let start_line_index = line_index(files, label.file_id, label.range.start)?;
            let start_line_number = line_number(files, label.file_id, start_line_index)?;
            let start_line_range = line_range(files, label.file_id, start_line_index)?;
            let end_line_index = line_index(files, label.file_id, label.range.end)?;
            let end_line_number = line_number(files, label.file_id, end_line_index)?;
            let end_line_range = line_range(files, label.file_id, end_line_index)?;

            outer_padding = core::cmp::max(outer_padding, count_digits(start_line_number));
            outer_padding = core::cmp::max(outer_padding, count_digits(end_line_number));

            // NOTE: This could be made more efficient by using an associative
            // data structure like a hashmap or B-tree,  but we use a vector to
            // preserve the order that unique files appear in the list of labels.
            let labeled_file = match labeled_files
                .iter_mut()
                .find(|labeled_file| label.file_id == labeled_file.file_id)
            {
                Some(labeled_file) => {
                    // another diagnostic also referenced this file
                    if labeled_file.start > label.range.start {
                        // this label has a higher style or has the same style but starts earlier
                        labeled_file.start = label.range.start;
                        labeled_file.location = location(files, label.file_id, label.range.start)?;
                    }
                    labeled_file
                }
                None => {
                    // no other diagnostic referenced this file yet
                    labeled_files.push(LabeledFile {
                        file_id: label.file_id,
                        start: label.range.start,
                        name: name(files, label.file_id)?.to_string(),
                        location: location(files, label.file_id, label.range.start)?,
                        num_multi_labels: 0,
                        lines: BTreeMap::new(),
                    });
                    // this unwrap should never fail because we just pushed an element
                    labeled_files.last_mut().unwrap()
                }
            };

            if start_line_index == end_line_index {
                // Single line
                //
                // ```text
                // 2 │ (+ test "")
                //   │         ^^ expected `Int` but found `String`
                // ```
                let label_start = label.range.start - start_line_range.start;
                // Ensure that we print at least one caret, even when we
                // have a zero-length source range.
                let label_end =
                    usize::max(label.range.end - start_line_range.start, label_start + 1);

                let line = labeled_file.get_or_insert_line(
                    start_line_index,
                    start_line_range,
                    start_line_number,
                );

                // Ensure that the single line labels are lexicographically
                // sorted by the range of source code that they cover.
                let index = match line.single_labels.binary_search_by(|(range, _)| {
                    // `Range<usize>` doesn't implement `Ord`, so convert to `(usize, usize)`
                    // to piggyback off its lexicographic comparison implementation.
                    (range.start, range.end).cmp(&(label_start, label_end))
                }) {
                    // If the ranges are the same, order the labels in reverse
                    // to how they were originally specified in the diagnostic.
                    // This helps with printing in the renderer.
                    Ok(index) | Err(index) => index,
                };

                line.single_labels
                    .insert(index, (label_start..label_end, &label.message));

                // If this line is not rendered, the SingleLabel is not visible.
                line.must_render = true;
            } else {
                // Multiple lines
                //
                // ```text
                // 4 │   fizz₁ num = case (mod num 5) (mod num 3) of
                //   │ ╭─────────────^
                // 5 │ │     0 0 => "FizzBuzz"
                // 6 │ │     0 _ => "Fizz"
                // 7 │ │     _ 0 => "Buzz"
                // 8 │ │     _ _ => num
                //   │ ╰──────────────^ `case` clauses have incompatible types
                // ```

                let label_index = labeled_file.num_multi_labels;
                labeled_file.num_multi_labels += 1;

                // First labeled line
                let label_start = label.range.start - start_line_range.start;

                let start_line = labeled_file.get_or_insert_line(
                    start_line_index,
                    start_line_range.clone(),
                    start_line_number,
                );

                start_line
                    .multi_labels
                    .push((label_index, MultiLabel::Top(label_start)));

                // The first line has to be rendered so the start of the label is visible.
                start_line.must_render = true;

                // Marked lines
                //
                // ```text
                // 5 │ │     0 0 => "FizzBuzz"
                // 6 │ │     0 _ => "Fizz"
                // 7 │ │     _ 0 => "Buzz"
                // ```
                for line_index in (start_line_index + 1)..end_line_index {
                    let line_range = line_range(files, label.file_id, line_index)?;
                    let line_number = line_number(files, label.file_id, line_index)?;

                    outer_padding = core::cmp::max(outer_padding, count_digits(line_number));

                    let line = labeled_file.get_or_insert_line(line_index, line_range, line_number);

                    line.multi_labels.push((label_index, MultiLabel::Left));

                    // The line should be rendered to match the configuration of how much context to show.
                    // Is this line part of the context after the start of the label?
                    // Is this line part of the context before the end of the label?
                    line.must_render |=
                        line_index - start_line_index <= 3 || end_line_index - line_index <= 1
                }

                // Last labeled line
                //
                // ```text
                // 8 │ │     _ _ => num
                //   │ ╰──────────────^ `case` clauses have incompatible types
                // ```
                let label_end = label.range.end - end_line_range.start;

                let end_line = labeled_file.get_or_insert_line(
                    end_line_index,
                    end_line_range,
                    end_line_number,
                );

                end_line
                    .multi_labels
                    .push((label_index, MultiLabel::Bottom(label_end, &label.message)));

                // The last line has to be rendered so the end of the label is visible.
                end_line.must_render = true;
            }
        }

        // Header and message
        //
        // ```text
        // error[E0001]: unexpected type in `+` application
        // ```
        renderer.render_header(None, self.message.as_str())?;

        // Source snippets
        //
        // ```text
        //   ┌─ test:2:9
        //   │
        // 2 │ (+ test "")
        //   │         ^^ expected `Int` but found `String`
        //   │
        // ```
        let mut labeled_files = labeled_files.into_iter().peekable();
        while let Some(labeled_file) = labeled_files.next() {
            let source = source(files, labeled_file.file_id)?;

            // Top left border and locus.
            //
            // ```text
            // ┌─ test:2:9
            // ```
            if !labeled_file.lines.is_empty() {
                renderer.render_snippet_start(
                    outer_padding,
                    &Locus {
                        name: labeled_file.name,
                        location: labeled_file.location,
                    },
                )?;
                renderer.render_snippet_empty(outer_padding, labeled_file.num_multi_labels, &[])?;
            }

            let mut lines = labeled_file
                .lines
                .iter()
                .filter(|(_, line)| line.must_render)
                .peekable();

            while let Some((line_index, line)) = lines.next() {
                renderer.render_snippet_source(
                    outer_padding,
                    line.number,
                    &source[line.range.clone()],
                    &line.single_labels,
                    labeled_file.num_multi_labels,
                    &line.multi_labels,
                )?;

                // Check to see if we need to render any intermediate stuff
                // before rendering the next line.
                if let Some((next_line_index, _)) = lines.peek() {
                    match next_line_index.checked_sub(*line_index) {
                        // Consecutive lines
                        Some(1) => {}
                        // One line between the current line and the next line
                        Some(2) => {
                            // Write a source line
                            let file_id = labeled_file.file_id;

                            // This line was not intended to be rendered initially.
                            // To render the line right, we have to get back the original labels.
                            let labels = labeled_file
                                .lines
                                .get(&(line_index + 1))
                                .map_or(&[][..], |line| &line.multi_labels[..]);

                            renderer.render_snippet_source(
                                outer_padding,
                                line_number(files, file_id, line_index + 1)?,
                                &source[line_range(files, file_id, line_index + 1)?],
                                &[],
                                labeled_file.num_multi_labels,
                                labels,
                            )?;
                        }
                        // More than one line between the current line and the next line.
                        Some(_) | None => {
                            // Source break
                            //
                            // ```text
                            // ·
                            // ```
                            renderer.render_snippet_break(
                                outer_padding,
                                labeled_file.num_multi_labels,
                                &line.multi_labels,
                            )?;
                        }
                    }
                }
            }

            // Check to see if we should render a trailing border after the
            // final line of the snippet.
            if labeled_files.peek().is_none() && self.notes.is_empty() {
                // We don't render a border if we are at the final newline
                // without trailing notes, because it would end up looking too
                // spaced-out in combination with the final new line.
            } else {
                // Render the trailing snippet border.
                renderer.render_snippet_empty(outer_padding, labeled_file.num_multi_labels, &[])?;
            }
        }

        // Additional notes
        //
        // ```text
        // = expected type `Int`
        //      found type `String`
        // ```
        for note in &self.notes {
            renderer.render_snippet_note(outer_padding, note)?;
        }
        renderer.render_empty()
    }
}

/// The 'location focus' of a source code snippet.
pub struct Locus {
    /// The user-facing name of the file.
    pub name: String,
    /// The location.
    pub location: Location,
}

/// Single-line label, with an optional message.
///
/// ```text
/// ^^^^^^^^^ blah blah
/// ```
pub type SingleLabel<'diagnostic> = (Range<usize>, &'diagnostic str);

/// A multi-line label to render.
///
/// Locations are relative to the start of where the source code is rendered.
pub enum MultiLabel<'diagnostic> {
    /// Multi-line label top.
    /// The contained value indicates where the label starts.
    ///
    /// ```text
    /// ╭────────────^
    /// ```
    ///
    /// Can also be rendered at the beginning of the line
    /// if there is only whitespace before the label starts.
    ///
    /// ```text
    /// ╭
    /// ```
    Top(usize),
    /// Left vertical labels for multi-line labels.
    ///
    /// ```text
    /// │
    /// ```
    Left,
    /// Multi-line label bottom, with an optional message.
    /// The first value indicates where the label ends.
    ///
    /// ```text
    /// ╰────────────^ blah blah
    /// ```
    Bottom(usize, &'diagnostic str),
}

#[derive(Copy, Clone)]
enum VerticalBound {
    Top,
    Bottom,
}

type Underline = VerticalBound;

/// A renderer of display list entries.
///
/// The following diagram gives an overview of each of the parts of the renderer's output:
///
/// ```text
///                     ┌ outer gutter
///                     │ ┌ left border
///                     │ │ ┌ inner gutter
///                     │ │ │   ┌─────────────────────────── source ─────────────────────────────┐
///                     │ │ │   │                                                                │
///                  ┌────────────────────────────────────────────────────────────────────────────
///        header ── │ error[0001]: oh noes, a cupcake has occurred!
/// snippet start ── │    ┌─ test:9:0
/// snippet empty ── │    │
///  snippet line ── │  9 │   ╭ Cupcake ipsum dolor. Sit amet marshmallow topping cheesecake
///  snippet line ── │ 10 │   │ muffin. Halvah croissant candy canes bonbon candy. Apple pie jelly
///                  │    │ ╭─│─────────^
/// snippet break ── │    · │ │
///  snippet line ── │ 33 │ │ │ Muffin danish chocolate soufflé pastry icing bonbon oat cake.
///  snippet line ── │ 34 │ │ │ Powder cake jujubes oat cake. Lemon drops tootsie roll marshmallow
///                  │    │ │ ╰─────────────────────────────^ blah blah
/// snippet break ── │    · │
///  snippet line ── │ 38 │ │   Brownie lemon drops chocolate jelly-o candy canes. Danish marzipan
///  snippet line ── │ 39 │ │   jujubes soufflé carrot cake marshmallow tiramisu caramels candy canes.
///                  │    │ │           ^^^^^^^^^^^^^^^^^^^ -------------------- blah blah
///                  │    │ │           │
///                  │    │ │           blah blah
///                  │    │ │           note: this is a note
///  snippet line ── │ 40 │ │   Fruitcake jelly-o danish toffee. Tootsie roll pastry cheesecake
///  snippet line ── │ 41 │ │   soufflé marzipan. Chocolate bar oat cake jujubes lollipop pastry
///  snippet line ── │ 42 │ │   cupcake. Candy canes cupcake toffee gingerbread candy canes muffin
///                  │    │ │                                ^^^^^^^^^^^^^^^^^^ blah blah
///                  │    │ ╰──────────^ blah blah
/// snippet break ── │    ·
///  snippet line ── │ 82 │     gingerbread toffee chupa chups chupa chups jelly-o cotton candy.
///                  │    │                 ^^^^^^                         ------- blah blah
/// snippet empty ── │    │
///  snippet note ── │    = blah blah
///  snippet note ── │    = blah blah blah
///                  │      blah blah
///  snippet note ── │    = blah blah blah
///                  │      blah blah
///         empty ── │
/// ```
///
/// Filler text from http://www.cupcakeipsum.com
pub struct Renderer<'writer, W>
where
    W: Write,
{
    writer: &'writer mut W,
}

impl<'writer, W: Write> Renderer<'writer, W> {
    /// Construct a renderer from the given writer and config.
    pub fn new(writer: &'writer mut W) -> Self {
        Renderer { writer }
    }

    /// Diagnostic header, with code, and message.
    ///
    /// ```text
    /// error[E0001]: unexpected type in `+` application
    /// ```
    pub fn render_header(&mut self, locus: Option<&Locus>, message: &str) -> Result {
        // Write locus
        //
        // ```text
        // test:2:9:
        // ```
        if let Some(locus) = locus {
            self.snippet_locus(locus)?;
            write!(self, ": ")?;
        }

        // Write diagnostic message
        //
        // ```text
        // : unexpected type in `+` application
        // ```
        write!(self, "{}", message)?;
        writeln!(self)?;

        Ok(())
    }

    /// Empty line.
    pub fn render_empty(&mut self) -> Result {
        return writeln!(self);
    }

    /// Top left border and locus.
    ///
    /// ```text
    /// ┌─ test:2:9
    /// ```
    pub fn render_snippet_start(&mut self, outer_padding: usize, locus: &Locus) -> Result {
        self.outer_gutter(outer_padding)?;

        write!(self, "{}", '┌')?;
        write!(self, "{0}", '─')?;

        write!(self, " ")?;
        self.snippet_locus(&locus)?;

        return writeln!(self);
    }

    /// A line of source code.
    ///
    /// ```text
    /// 10 │   │ muffin. Halvah croissant candy canes bonbon candy. Apple pie jelly
    ///    │ ╭─│─────────^
    /// ```
    pub fn render_snippet_source(
        &mut self,
        outer_padding: usize,
        line_number: usize,
        source: &str,
        single_labels: &[SingleLabel<'_>],
        num_multi_labels: usize,
        multi_labels: &[(usize, MultiLabel<'_>)],
    ) -> Result {
        // Trim trailing newlines, linefeeds, and null chars from source, if they exist.
        // FIXME: Use the number of trimmed placeholders when rendering single line carets
        let source = source.trim_end_matches(['\n', '\r', '\0'].as_ref());

        // Write source line
        //
        // ```text
        // 10 │   │ muffin. Halvah croissant candy canes bonbon candy. Apple pie jelly
        // ```
        {
            // Write outer gutter (with line number) and border
            self.outer_gutter_number(line_number, outer_padding)?;
            self.border_left()?;

            // Write inner gutter (with multi-line continuations on the left if necessary)
            let mut multi_labels_iter = multi_labels.iter().peekable();
            for label_column in 0..num_multi_labels {
                match multi_labels_iter.peek() {
                    Some((label_index, label)) if *label_index == label_column => {
                        match label {
                            MultiLabel::Top(start)
                                if *start <= source.len() - source.trim_start().len() =>
                            {
                                self.label_multi_top_left()?;
                            }
                            MultiLabel::Top(..) => self.inner_gutter_space()?,
                            MultiLabel::Left | MultiLabel::Bottom(..) => {
                                self.label_multi_left(false)?;
                            }
                        }
                        multi_labels_iter.next();
                    }
                    Some((_, _)) | None => self.inner_gutter_space()?,
                }
            }

            // Write source text
            write!(self, " ")?;
            for (metrics, ch) in self.char_metrics(source.char_indices()) {
                let column_range = metrics.byte_index..(metrics.byte_index + ch.len_utf8());

                match ch {
                    '\t' => (0..metrics.unicode_width).try_for_each(|_| write!(self, " "))?,
                    _ => write!(self, "{}", ch)?,
                }
            }
            writeln!(self)?;
        }

        // Write single labels underneath source
        //
        // ```text
        //   │     - ---- ^^^ second mutable borrow occurs here
        //   │     │ │
        //   │     │ first mutable borrow occurs here
        //   │     first borrow later used by call
        //   │     help: some help here
        // ```
        if !single_labels.is_empty() {
            // Our plan is as follows:
            //
            // 1. Do an initial scan to find:
            //    - The number of non-empty messages.
            //    - The right-most start and end positions of labels.
            //    - A candidate for a trailing label (where the label's message
            //      is printed to the left of the caret).
            // 2. Check if the trailing label candidate overlaps another label -
            //    if so we print it underneath the carets with the other labels.
            // 3. Print a line of carets, and (possibly) the trailing message
            //    to the left.
            // 4. Print vertical lines pointing to the carets, and the messages
            //    for those carets.
            //
            // We try our best avoid introducing new dynamic allocations,
            // instead preferring to iterate over the labels multiple times. It
            // is unclear what the performance tradeoffs are however, so further
            // investigation may be required.

            // The number of non-empty messages to print.
            let mut num_messages = 0;
            // The right-most start position, eg:
            //
            // ```text
            // -^^^^---- ^^^^^^^
            //           │
            //           right-most start position
            // ```
            let mut max_label_start = 0;
            // The right-most end position, eg:
            //
            // ```text
            // -^^^^---- ^^^^^^^
            //                 │
            //                 right-most end position
            // ```
            let mut max_label_end = 0;
            // A trailing message, eg:
            //
            // ```text
            // ^^^ second mutable borrow occurs here
            // ```
            let mut trailing_label = None;

            for (label_index, label) in single_labels.iter().enumerate() {
                let (range, message) = label;
                if !message.is_empty() {
                    num_messages += 1;
                }
                max_label_start = core::cmp::max(max_label_start, range.start);
                max_label_end = core::cmp::max(max_label_end, range.end);
                // This is a candidate for the trailing label, so let's record it.
                if range.end == max_label_end {
                    if message.is_empty() {
                        trailing_label = None;
                    } else {
                        trailing_label = Some((label_index, label));
                    }
                }
            }
            if let Some((trailing_label_index, (trailing_range, _))) = trailing_label {
                // Check to see if the trailing label candidate overlaps any of
                // the other labels on the current line.
                if single_labels
                    .iter()
                    .enumerate()
                    .filter(|(label_index, _)| *label_index != trailing_label_index)
                    .any(|(_, (range, _))| is_overlapping(trailing_range, range))
                {
                    // If it does, we'll instead want to render it below the
                    // carets along with the other hanging labels.
                    trailing_label = None;
                }
            }

            // Write a line of carets
            //
            // ```text
            //   │ ^^^^^^  -------^^^^^^^^^-------^^^^^----- ^^^^ trailing label message
            // ```
            self.outer_gutter(outer_padding)?;
            self.border_left()?;
            self.inner_gutter(num_multi_labels, multi_labels)?;
            write!(self, " ")?;

            let placeholder_metrics = Metrics {
                byte_index: source.len(),
                unicode_width: 1,
            };
            for (metrics, ch) in self
                .char_metrics(source.char_indices())
                // Add a placeholder source column at the end to allow for
                // printing carets at the end of lines, eg:
                //
                // ```text
                // 1 │ Hello world!
                //   │             ^
                // ```
                .chain(core::iter::once((placeholder_metrics, '\0')))
            {
                // Find the current label style at this column
                let column_range = metrics.byte_index..(metrics.byte_index + ch.len_utf8());
                let caret_iter = single_labels
                    .iter()
                    .filter(|(range, _)| is_overlapping(range, &column_range));
                let caret_ch = caret_iter.map(|_| '^').next();

                let caret_ch = match caret_ch {
                    // Only print padding if we are before the end of the last single line caret
                    None if metrics.byte_index < max_label_end => Some(' '),
                    x => x,
                };
                if let Some(caret_ch) = caret_ch {
                    // FIXME: improve rendering of carets between character boundaries
                    (0..metrics.unicode_width).try_for_each(|_| write!(self, "{}", caret_ch))?;
                }
            }

            // Write first trailing label message
            if let Some((_, (_, message))) = trailing_label {
                write!(self, " ")?;
                write!(self, "{}", message)?;
            }
            writeln!(self)?;

            // Write hanging labels pointing to carets
            //
            // ```text
            //   │     │ │
            //   │     │ first mutable borrow occurs here
            //   │     first borrow later used by call
            //   │     help: some help here
            // ```
            if num_messages > trailing_label.iter().count() {
                // Write first set of vertical lines before hanging labels
                //
                // ```text
                //   │     │ │
                // ```
                self.outer_gutter(outer_padding)?;
                self.border_left()?;
                self.inner_gutter(num_multi_labels, multi_labels)?;
                write!(self, " ")?;
                self.caret_pointers(
                    max_label_start,
                    single_labels,
                    trailing_label,
                    source.char_indices(),
                )?;
                writeln!(self)?;

                // Write hanging labels pointing to carets
                //
                // ```text
                //   │     │ first mutable borrow occurs here
                //   │     first borrow later used by call
                //   │     help: some help here
                // ```
                for (range, message) in hanging_labels(single_labels, trailing_label).rev() {
                    self.outer_gutter(outer_padding)?;
                    self.border_left()?;
                    self.inner_gutter(num_multi_labels, multi_labels)?;
                    write!(self, " ")?;
                    self.caret_pointers(
                        max_label_start,
                        single_labels,
                        trailing_label,
                        source
                            .char_indices()
                            .take_while(|(byte_index, _)| *byte_index < range.start),
                    )?;
                    write!(self, "{}", message)?;
                    writeln!(self)?;
                }
            }
        }

        // Write top or bottom label carets underneath source
        //
        // ```text
        //     │ ╰───│──────────────────^ woops
        //     │   ╭─│─────────^
        // ```
        for (multi_label_index, (_, label)) in multi_labels.iter().enumerate() {
            let (range, bottom_message) = match label {
                MultiLabel::Left => continue, // no label caret needed
                // no label caret needed if this can be started in front of the line
                MultiLabel::Top(start) if *start <= source.len() - source.trim_start().len() => {
                    continue
                }
                MultiLabel::Top(range) => (range, None),
                MultiLabel::Bottom(range, message) => (range, Some(message)),
            };

            self.outer_gutter(outer_padding)?;
            self.border_left()?;

            // Write inner gutter.
            //
            // ```text
            //  │ ╭─│───│
            // ```
            let mut underline = None;
            let mut multi_labels_iter = multi_labels.iter().enumerate().peekable();
            for label_column in 0..num_multi_labels {
                match multi_labels_iter.peek() {
                    Some((i, (label_index, label))) if *label_index == label_column => {
                        match label {
                            MultiLabel::Left => {
                                self.label_multi_left(underline.is_some())?;
                            }
                            MultiLabel::Top(..) if multi_label_index > *i => {
                                self.label_multi_left(underline.is_some())?;
                            }
                            MultiLabel::Bottom(..) if multi_label_index < *i => {
                                self.label_multi_left(underline.is_some())?;
                            }
                            MultiLabel::Top(..) if multi_label_index == *i => {
                                underline = Some(VerticalBound::Top);
                                self.label_multi_top_left()?
                            }
                            MultiLabel::Bottom(..) if multi_label_index == *i => {
                                underline = Some(VerticalBound::Bottom);
                                self.label_multi_bottom_left()?;
                            }
                            MultiLabel::Top(..) | MultiLabel::Bottom(..) => {
                                self.inner_gutter_column(underline)?;
                            }
                        }
                        multi_labels_iter.next();
                    }
                    Some((_, _)) | None => self.inner_gutter_column(underline)?,
                }
            }

            // Finish the top or bottom caret
            match bottom_message {
                None => self.label_multi_top_caret(source, *range)?,
                Some(message) => self.label_multi_bottom_caret(source, *range, message)?,
            }
        }

        Ok(())
    }

    /// An empty source line, for providing additional whitespace to source snippets.
    ///
    /// ```text
    /// │ │ │
    /// ```
    pub fn render_snippet_empty(
        &mut self,
        outer_padding: usize,
        num_multi_labels: usize,
        multi_labels: &[(usize, MultiLabel<'_>)],
    ) -> Result {
        self.outer_gutter(outer_padding)?;
        self.border_left()?;
        self.inner_gutter(num_multi_labels, multi_labels)?;
        return writeln!(self);
    }

    /// A broken source line, for labeling skipped sections of source.
    ///
    /// ```text
    /// · │ │
    /// ```
    pub fn render_snippet_break(
        &mut self,
        outer_padding: usize,
        num_multi_labels: usize,
        multi_labels: &[(usize, MultiLabel<'_>)],
    ) -> Result {
        self.outer_gutter(outer_padding)?;
        self.border_left_break()?;
        self.inner_gutter(num_multi_labels, multi_labels)?;
        return writeln!(self);
    }

    /// Additional notes.
    ///
    /// ```text
    /// = expected type `Int`
    ///      found type `String`
    /// ```
    pub fn render_snippet_note(&mut self, outer_padding: usize, message: &str) -> Result {
        for (note_line_index, line) in message.lines().enumerate() {
            self.outer_gutter(outer_padding)?;
            match note_line_index {
                0 => write!(self, "{}", '=')?,
                _ => write!(self, " ")?,
            }
            // Write line of message
            writeln!(self, " {}", line)?;
        }

        Ok(())
    }

    /// Adds tab-stop aware unicode-width computations to an iterator over
    /// character indices. Assumes that the character indices begin at the start
    /// of the line.
    fn char_metrics(
        &self,
        char_indices: impl Iterator<Item = (usize, char)>,
    ) -> impl Iterator<Item = (Metrics, char)> {
        use unicode_width::UnicodeWidthChar;

        let tab_width = 2;
        let mut unicode_column = 0;

        char_indices.map(move |(byte_index, ch)| {
            let metrics = Metrics {
                byte_index,
                unicode_width: match (ch, tab_width) {
                    ('\t', 0) => 0, // Guard divide-by-zero
                    ('\t', _) => tab_width - (unicode_column % tab_width),
                    (ch, _) => ch.width().unwrap_or(0),
                },
            };
            unicode_column += metrics.unicode_width;

            (metrics, ch)
        })
    }

    /// Location focus.
    fn snippet_locus(&mut self, locus: &Locus) -> Result {
        write!(
            self,
            "{name}:{line_number}:{column_number}",
            name = locus.name,
            line_number = locus.location.line_number,
            column_number = locus.location.column_number,
        )?;
        Ok(())
    }

    /// The outer gutter of a source line.
    fn outer_gutter(&mut self, outer_padding: usize) -> Result {
        write!(self, "{space: >width$} ", space = "", width = outer_padding)?;
        Ok(())
    }

    /// The outer gutter of a source line, with line number.
    fn outer_gutter_number(&mut self, line_number: usize, outer_padding: usize) -> Result {
        write!(
            self,
            "{line_number: >width$}",
            line_number = line_number,
            width = outer_padding,
        )?;
        write!(self, " ")?;
        Ok(())
    }

    /// The left-hand border of a source line.
    fn border_left(&mut self) -> Result {
        return write!(self, "{}", '|');
    }

    /// The broken left-hand border of a source line.
    fn border_left_break(&mut self) -> Result {
        return write!(self, "{}", '·');
    }

    /// Write vertical lines pointing to carets.
    fn caret_pointers(
        &mut self,
        max_label_start: usize,
        single_labels: &[SingleLabel<'_>],
        trailing_label: Option<(usize, &SingleLabel<'_>)>,
        char_indices: impl Iterator<Item = (usize, char)>,
    ) -> Result {
        for (metrics, ch) in self.char_metrics(char_indices) {
            let column_range = metrics.byte_index..(metrics.byte_index + ch.len_utf8());
            let label_style = hanging_labels(single_labels, trailing_label)
                .filter(|(range, _)| column_range.contains(&range.start))
                .next();

            let mut spaces = match label_style {
                None => 0..metrics.unicode_width,
                Some(label_style) => {
                    write!(self, "{}", '|')?;
                    1..metrics.unicode_width
                }
            };
            // Only print padding if we are before the end of the last single line caret
            if metrics.byte_index <= max_label_start {
                spaces.try_for_each(|_| write!(self, " "))?;
            }
        }

        Ok(())
    }

    /// The left of a multi-line label.
    ///
    /// ```text
    ///  │
    /// ```
    fn label_multi_left(&mut self, underline: bool) -> Result {
        match underline {
            false => write!(self, " ")?,
            true => write!(self, "{}", '-')?,
        }

        return write!(self, "{}", '|');
    }

    /// The top-left of a multi-line label.
    ///
    /// ```text
    ///  ╭
    /// ```
    fn label_multi_top_left(&mut self) -> Result {
        return write!(self, " {}", '╭');
    }

    /// The bottom left of a multi-line label.
    ///
    /// ```text
    ///  ╰
    /// ```
    fn label_multi_bottom_left(&mut self) -> Result {
        return write!(self, " {}", '╰');
    }

    /// Multi-line label top.
    ///
    /// ```text
    /// ─────────────^
    /// ```
    fn label_multi_top_caret(&mut self, source: &str, start: usize) -> Result {
        for (metrics, _) in self
            .char_metrics(source.char_indices())
            .take_while(|(metrics, _)| metrics.byte_index < start + 1)
        {
            // FIXME: improve rendering of carets between character boundaries
            (0..metrics.unicode_width).try_for_each(|_| write!(self, "{}", '-'))?;
        }

        return writeln!(self, "{}", '^');
    }

    /// Multi-line label bottom, with a message.
    ///
    /// ```text
    /// ─────────────^ expected `Int` but found `String`
    /// ```
    fn label_multi_bottom_caret(&mut self, source: &str, start: usize, message: &str) -> Result {
        for (metrics, _) in self
            .char_metrics(source.char_indices())
            .take_while(|(metrics, _)| metrics.byte_index < start)
        {
            // FIXME: improve rendering of carets between character boundaries
            (0..metrics.unicode_width).try_for_each(|_| write!(self, "{}", '-'))?;
        }

        write!(self, "{}", '^')?;
        if !message.is_empty() {
            write!(self, " {}", message)?;
        }
        writeln!(self)?;
        Ok(())
    }

    /// Writes an empty gutter space, or continues an underline horizontally.
    fn inner_gutter_column(&mut self, underline: Option<Underline>) -> Result {
        match underline {
            None => self.inner_gutter_space(),
            Some(vertical_bound) => {
                let ch = match vertical_bound {
                    VerticalBound::Top => '-',
                    VerticalBound::Bottom => '-',
                };
                write!(self, "{0}{0}", ch)?;
                Ok(())
            }
        }
    }

    /// Writes an empty gutter space.
    fn inner_gutter_space(&mut self) -> Result {
        write!(self, "  ")?;
        Ok(())
    }

    /// Writes an inner gutter, with the left lines if necessary.
    fn inner_gutter(
        &mut self,
        num_multi_labels: usize,
        multi_labels: &[(usize, MultiLabel<'_>)],
    ) -> Result {
        let mut multi_labels_iter = multi_labels.iter().peekable();
        for label_column in 0..num_multi_labels {
            match multi_labels_iter.peek() {
                Some((label_index, label)) if *label_index == label_column => match label {
                    MultiLabel::Left | MultiLabel::Bottom(..) => {
                        self.label_multi_left(false)?;
                        multi_labels_iter.next();
                    }
                    MultiLabel::Top(..) => {
                        self.inner_gutter_space()?;
                        multi_labels_iter.next();
                    }
                },
                Some((_, _)) | None => self.inner_gutter_space()?,
            }
        }

        Ok(())
    }
}

impl<'writer, W: Write> Write for Renderer<'writer, W> {
    fn write_str(&mut self, buf: &str) -> Result {
        self.writer.write_str(buf)
    }
}

struct Metrics {
    byte_index: usize,
    unicode_width: usize,
}

/// Check if two ranges overlap
fn is_overlapping(range0: &Range<usize>, range1: &Range<usize>) -> bool {
    let start = core::cmp::max(range0.start, range1.start);
    let end = core::cmp::min(range0.end, range1.end);
    start < end
}

/// Return an iterator that yields the labels that require hanging messages
/// rendered underneath them.
fn hanging_labels<'labels, 'diagnostic>(
    single_labels: &'labels [SingleLabel<'diagnostic>],
    trailing_label: Option<(usize, &'labels SingleLabel<'diagnostic>)>,
) -> impl 'labels + DoubleEndedIterator<Item = &'labels SingleLabel<'diagnostic>> {
    single_labels
        .iter()
        .enumerate()
        .filter(|(_, (_, message))| !message.is_empty())
        .filter(move |(i, _)| trailing_label.map_or(true, |(j, _)| *i != j))
        .map(|(_, label)| label)
}

fn line_index(files: &FileDb, id: u32, idx: usize) -> core::result::Result<usize, Error> {
    files.line_index(id, idx).ok_or(Error)
}

fn line_range(files: &FileDb, id: u32, idx: usize) -> core::result::Result<Range<usize>, Error> {
    files.line_range(id, idx).ok_or(Error)
}

fn line_number(files: &FileDb, id: u32, idx: usize) -> core::result::Result<usize, Error> {
    files.line_number(id, idx).ok_or(Error)
}

fn location(files: &FileDb, id: u32, idx: usize) -> core::result::Result<Location, Error> {
    files.location(id, idx).ok_or(Error)
}

fn name(files: &FileDb, id: u32) -> core::result::Result<&str, Error> {
    files.name(id).ok_or(Error)
}

fn source(files: &FileDb, id: u32) -> core::result::Result<&str, Error> {
    files.source(id).ok_or(Error)
}
