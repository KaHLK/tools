use crate::prelude::*;
use crate::utils::JsAnyBinaryLikeExpression;
use rome_formatter::write;

use rome_js_syntax::JsLogicalExpression;

#[derive(Debug, Clone, Default)]
pub struct FormatJsLogicalExpression;

impl FormatNodeRule<JsLogicalExpression> for FormatJsLogicalExpression {
    fn fmt_fields(&self, node: &JsLogicalExpression, f: &mut JsFormatter) -> FormatResult<()> {
        write!(
            f,
            [JsAnyBinaryLikeExpression::JsLogicalExpression(node.clone())]
        )
    }
}
