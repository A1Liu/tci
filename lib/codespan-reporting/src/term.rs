//! Terminal back-end for emitting diagnostics.

use core::fmt;

use crate::diagnostic::Diagnostic;
use crate::files::Files;

mod config;
mod renderer;
mod views;

pub use termcolor;

pub use self::config::{Chars, Config, DisplayStyle, Styles};

/// Emit a diagnostic using the given writer, context, config, and files.
pub fn emit<'files, F: Files<'files>>(
    writer: &mut impl fmt::Write,
    files: &'files F,
    diagnostic: &Diagnostic<F::FileId>,
) -> fmt::Result {
    use self::renderer::Renderer;
    use self::views::ShortDiagnostic;

    let mut renderer = Renderer::new(writer);
    ShortDiagnostic::new(diagnostic).render(files, &mut renderer)
}
