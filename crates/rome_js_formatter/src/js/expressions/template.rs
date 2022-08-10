use crate::prelude::*;
use rome_formatter::write;

use crate::parentheses::NeedsParentheses;
use rome_js_syntax::JsTemplateFields;
use rome_js_syntax::{JsSyntaxNode, JsTemplate};

#[derive(Debug, Clone, Default)]
pub struct FormatJsTemplate;

impl FormatNodeRule<JsTemplate> for FormatJsTemplate {
    fn fmt_fields(&self, node: &JsTemplate, f: &mut JsFormatter) -> FormatResult<()> {
        let JsTemplateFields {
            tag,
            type_arguments,
            l_tick_token,
            elements,
            r_tick_token,
        } = node.as_fields();

        write![
            f,
            [
                tag.format(),
                type_arguments.format(),
                line_suffix_boundary(),
                l_tick_token.format(),
                elements.format(),
                r_tick_token.format()
            ]
        ]
    }

    fn needs_parentheses(&self, item: &JsTemplate) -> bool {
        item.needs_parentheses()
    }
}

/// `TemplateLiteral`'s are `PrimaryExpression's that never need parentheses.
impl NeedsParentheses for JsTemplate {
    fn needs_parentheses(&self) -> bool {
        false
    }

    fn needs_parentheses_with_parent(&self, _parent: &JsSyntaxNode) -> bool {
        false
    }
}
