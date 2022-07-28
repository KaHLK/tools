use crate::prelude::*;
use crate::utils::JsAnyBinaryLikeExpression;
use rome_formatter::write;

use rome_js_syntax::JsBinaryExpression;

#[derive(Debug, Clone, Default)]
pub struct FormatJsBinaryExpression;

impl FormatNodeRule<JsBinaryExpression> for FormatJsBinaryExpression {
    fn fmt_fields(&self, node: &JsBinaryExpression, f: &mut JsFormatter) -> FormatResult<()> {
        write!(
            f,
            [JsAnyBinaryLikeExpression::JsBinaryExpression(node.clone())]
        )
    }
}
