use crate::{IndentStyle, LineWidth};

/// Options that affect how the [crate::Printer] prints the format tokens
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PrinterOptions {
    /// Width of a single tab character (does it equal 2, 4, ... spaces?)
    pub tab_width: u8,

    /// What's the max width of a line. Defaults to 80
    pub print_width: LineWidth,

    /// The type of line ending to apply to the printed input
    pub line_ending: LineEnding,

    /// Whether the printer should use tabs or spaces to indent code and if spaces, by how many.
    pub indent_style: IndentStyle,
}

impl PrinterOptions {
    pub fn with_print_width(mut self, width: LineWidth) -> Self {
        self.print_width = width;
        self
    }

    pub fn with_indent(mut self, style: IndentStyle) -> Self {
        self.indent_style = style;

        self
    }

    pub(crate) fn indent_style(&self) -> IndentStyle {
        self.indent_style
    }

    /// Width of an indent in characters.
    pub(super) const fn indent_width(&self) -> u8 {
        match self.indent_style {
            IndentStyle::Tab => self.tab_width,
            IndentStyle::Space(count) => count,
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LineEnding {
    ///  Line Feed only (\n), common on Linux and macOS as well as inside git repos
    LineFeed,

    /// Carriage Return + Line Feed characters (\r\n), common on Windows
    CarriageReturnLineFeed,

    /// Carriage Return character only (\r), used very rarely
    CarriageReturn,
}

impl LineEnding {
    #[inline]
    pub const fn as_str(&self) -> &'static str {
        match self {
            LineEnding::LineFeed => "\n",
            LineEnding::CarriageReturnLineFeed => "\r\n",
            LineEnding::CarriageReturn => "\r",
        }
    }
}

impl Default for PrinterOptions {
    fn default() -> Self {
        PrinterOptions {
            tab_width: 2,
            print_width: LineWidth::default(),
            indent_style: Default::default(),
            line_ending: LineEnding::LineFeed,
        }
    }
}
