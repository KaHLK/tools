use crate::prelude::*;
use crate::utils::JsAnyBinaryLikeExpression;
use rome_formatter::write;

use rome_js_syntax::JsInExpression;

#[derive(Debug, Clone, Default)]
pub struct FormatJsInExpression;

impl FormatNodeRule<JsInExpression> for FormatJsInExpression {
    fn fmt_fields(&self, node: &JsInExpression, f: &mut JsFormatter) -> FormatResult<()> {
        write!(f, [JsAnyBinaryLikeExpression::JsInExpression(node.clone())])
    }
}
