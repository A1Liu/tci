use crate::diagnostic::{Diagnostic, LabelStyle};
use crate::files::{Files, Location};
use crate::term::renderer::{Locus, MultiLabel, Renderer, SingleLabel};
use alloc::string::{String, ToString};
use core::fmt;
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

/// Output a short diagnostic, with a line number, severity, and message.
pub struct ShortDiagnostic<'diagnostic, FileId> {
    diagnostic: &'diagnostic Diagnostic<FileId>,
}

impl<'diagnostic, FileId> ShortDiagnostic<'diagnostic, FileId>
where
    FileId: Copy + PartialEq,
{
    pub fn new(
        diagnostic: &'diagnostic Diagnostic<FileId>,
    ) -> ShortDiagnostic<'diagnostic, FileId> {
        ShortDiagnostic { diagnostic }
    }

    pub fn render<'files, W>(
        &self,
        files: &'files impl Files<'files, FileId = FileId>,
        renderer: &mut Renderer<'_, W>,
    ) -> fmt::Result
    where
        FileId: 'files,
        W: fmt::Write,
    {
        // Located headers
        //
        // ```text
        // test:2:9: error[E0001]: unexpected type in `+` application
        // ```
        let mut primary_labels_encountered = 0;
        let labels = self.diagnostic.labels.iter();
        for label in labels.filter(|label| label.style == LabelStyle::Primary) {
            primary_labels_encountered += 1;

            renderer.render_header(
                Some(&Locus {
                    name: files.name(label.file_id).unwrap().to_string(),
                    location: files.location(label.file_id, label.range.start).unwrap(),
                }),
                self.diagnostic.severity,
                self.diagnostic.code.as_ref().map(String::as_str),
                self.diagnostic.message.as_str(),
            )?;
        }

        // Fallback to printing a non-located header if no primary labels were encountered
        //
        // ```text
        // error[E0002]: Bad config found
        // ```
        if primary_labels_encountered == 0 {
            renderer.render_header(
                None,
                self.diagnostic.severity,
                self.diagnostic.code.as_ref().map(String::as_str),
                self.diagnostic.message.as_str(),
            )?;
        }

        Ok(())
    }
}
