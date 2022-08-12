use crate::prelude::*;

use crate::js::expressions::static_member_expression::member_chain_callee_needs_parens;
use crate::parentheses::NeedsParentheses;
use rome_formatter::{format_args, write};
use rome_js_syntax::{JsComputedMemberExpression, JsSyntaxNode};
use rome_js_syntax::{JsComputedMemberExpressionFields, JsSyntaxKind};

#[derive(Debug, Clone, Default)]
pub struct FormatJsComputedMemberExpression;

impl FormatNodeRule<JsComputedMemberExpression> for FormatJsComputedMemberExpression {
    fn fmt_fields(
        &self,
        node: &JsComputedMemberExpression,
        f: &mut JsFormatter,
    ) -> FormatResult<()> {
        let JsComputedMemberExpressionFields {
            object,
            optional_chain_token,
            l_brack_token,
            member,
            r_brack_token,
        } = node.as_fields();

        write![
            f,
            [
                object.format(),
                group(&format_args![
                    optional_chain_token.format(),
                    l_brack_token.format(),
                    soft_line_break(),
                    soft_block_indent(&member.format()),
                    r_brack_token.format()
                ]),
            ]
        ]
    }

    fn needs_parentheses(&self, item: &JsComputedMemberExpression) -> bool {
        item.needs_parentheses()
    }
}

impl NeedsParentheses for JsComputedMemberExpression {
    fn needs_parentheses_with_parent(&self, parent: &JsSyntaxNode) -> bool {
        if self.is_optional_chain() && matches!(parent.kind(), JsSyntaxKind::JS_NEW_EXPRESSION) {
            return true;
        }

        member_chain_callee_needs_parens(self.clone().into(), parent)
    }
}

#[cfg(test)]
mod tests {

    use crate::{assert_needs_parentheses, assert_not_needs_parentheses};
    use rome_js_syntax::JsComputedMemberExpression;

    #[test]
    fn needs_parentheses() {
        assert_needs_parentheses!("new (test()[a])()", JsComputedMemberExpression);
        assert_needs_parentheses!("new (test().a[b])()", JsComputedMemberExpression);
        assert_needs_parentheses!(
            "new (test()`template`[index])()",
            JsComputedMemberExpression
        );
        assert_needs_parentheses!("new (test()![member])()", JsComputedMemberExpression);

        assert_needs_parentheses!("new (a?.b[c])()", JsComputedMemberExpression);
        assert_not_needs_parentheses!("new (test[a])()", JsComputedMemberExpression);
    }
}
