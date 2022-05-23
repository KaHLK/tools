use crate::prelude::*;
use crate::utils::format_with_semicolon;

use crate::utils::FormatStringLiteralToken;
use crate::FormatNodeFields;
use rome_js_syntax::JsDirective;
use rome_js_syntax::JsDirectiveFields;

impl FormatNodeFields<JsDirective> for FormatNodeRule<JsDirective> {
    fn format_fields(
        node: &JsDirective,
        formatter: &Formatter<JsFormatOptions>,
    ) -> FormatResult<FormatElement> {
        let JsDirectiveFields {
            value_token,
            semicolon_token,
        } = node.as_fields();

        format_with_semicolon(
            formatter,
            formatted![formatter, [FormatStringLiteralToken::new(value_token?)]]?,
            semicolon_token,
        )
    }
}
