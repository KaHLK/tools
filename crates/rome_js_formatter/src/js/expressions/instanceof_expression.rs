use crate::prelude::*;
use crate::utils::JsAnyBinaryLikeExpression;
use rome_formatter::write;

use rome_js_syntax::JsInstanceofExpression;

#[derive(Debug, Clone, Default)]
pub struct FormatJsInstanceofExpression;

impl FormatNodeRule<JsInstanceofExpression> for FormatJsInstanceofExpression {
    fn fmt_fields(&self, node: &JsInstanceofExpression, f: &mut JsFormatter) -> FormatResult<()> {
        write!(
            f,
            [JsAnyBinaryLikeExpression::JsInstanceofExpression(
                node.clone()
            )]
        )
    }
}
