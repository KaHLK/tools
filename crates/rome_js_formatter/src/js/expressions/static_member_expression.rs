use crate::prelude::*;

use crate::parentheses::NeedsParentheses;
use rome_formatter::{format_args, write};
use rome_js_syntax::{
    JsAnyExpression, JsAssignmentExpression, JsStaticMemberExpression,
    JsStaticMemberExpressionFields, JsSyntaxKind, JsSyntaxNode, JsVariableDeclarator,
};
use rome_rowan::AstNode;

#[derive(Debug, Clone, Default)]
pub struct FormatJsStaticMemberExpression;

impl FormatNodeRule<JsStaticMemberExpression> for FormatJsStaticMemberExpression {
    fn fmt_fields(&self, node: &JsStaticMemberExpression, f: &mut JsFormatter) -> FormatResult<()> {
        let JsStaticMemberExpressionFields {
            object,
            operator_token,
            member,
        } = node.as_fields();

        write!(f, [object.format()])?;

        let layout = compute_member_layout(node)?;

        match layout {
            StaticMemberExpressionLayout::NoBreak => {
                write!(f, [operator_token.format(), member.format()])
            }
            StaticMemberExpressionLayout::BreakAfterObject => {
                write!(
                    f,
                    [group(&indent(&format_args![
                        soft_line_break(),
                        operator_token.format(),
                        member.format(),
                    ]))]
                )
            }
        }
    }

    fn needs_parentheses(&self, item: &JsStaticMemberExpression) -> bool {
        item.needs_parentheses()
    }
}

enum StaticMemberExpressionLayout {
    /// Forces that there's no line break between the object, operator, and member
    NoBreak,

    /// Breaks the static member expression after the object if the whole expression doesn't fit on a single line
    BreakAfterObject,
}

fn compute_member_layout(
    member: &JsStaticMemberExpression,
) -> FormatResult<StaticMemberExpressionLayout> {
    let parent = member.syntax().parent();

    let nested = parent
        .as_ref()
        .map_or(false, |p| JsStaticMemberExpression::can_cast(p.kind()));

    if let Some(parent) = &parent {
        if JsAssignmentExpression::can_cast(parent.kind())
            || JsVariableDeclarator::can_cast(parent.kind())
        {
            let no_break = match member.object()? {
                JsAnyExpression::JsCallExpression(call_expression) => {
                    !call_expression.arguments()?.args().is_empty()
                }
                JsAnyExpression::TsNonNullAssertionExpression(non_null_assertion) => {
                    match non_null_assertion.expression()? {
                        JsAnyExpression::JsCallExpression(call_expression) => {
                            !call_expression.arguments()?.args().is_empty()
                        }
                        _ => false,
                    }
                }
                _ => false,
            };

            if no_break {
                return Ok(StaticMemberExpressionLayout::NoBreak);
            }
        }
    };

    if !nested && matches!(member.object()?, JsAnyExpression::JsIdentifierExpression(_)) {
        return Ok(StaticMemberExpressionLayout::NoBreak);
    }

    let first_non_static_member_ancestor = member
        .syntax()
        .ancestors()
        .find(|parent| !JsStaticMemberExpression::can_cast(parent.kind()));

    if matches!(
        first_non_static_member_ancestor.and_then(JsAnyExpression::cast),
        Some(JsAnyExpression::JsNewExpression(_))
    ) {
        return Ok(StaticMemberExpressionLayout::NoBreak);
    }

    Ok(StaticMemberExpressionLayout::BreakAfterObject)
}

impl NeedsParentheses for JsStaticMemberExpression {
    fn needs_parentheses_with_parent(&self, parent: &JsSyntaxNode) -> bool {
        if self.is_optional_chain() && matches!(parent.kind(), JsSyntaxKind::JS_NEW_EXPRESSION) {
            return true;
        }

        memberish_needs_parens(self.clone().into(), parent)
    }
}

pub(crate) fn memberish_needs_parens(node: JsAnyExpression, parent: &JsSyntaxNode) -> bool {
    use JsAnyExpression::*;
    debug_assert!(
        matches!(
            node,
            JsStaticMemberExpression(_)
                | JsComputedMemberExpression(_)
                | TsNonNullAssertionExpression(_)
        ),
        "Expected node to be a member expression"
    );

    match parent.kind() {
        // `new (test().a)
        JsSyntaxKind::JS_NEW_EXPRESSION => {
            let mut object_chain =
                std::iter::successors(Some(node), |expression| match expression {
                    JsStaticMemberExpression(member) => member.object().ok(),
                    JsComputedMemberExpression(member) => member.object().ok(),
                    JsTemplate(template) => template.tag(),
                    TsNonNullAssertionExpression(assertion) => assertion.expression().ok(),
                    _ => None,
                });

            object_chain.any(|object| matches!(object, JsCallExpression(_)))
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {

    use crate::{assert_needs_parentheses, assert_not_needs_parentheses};
    use rome_js_syntax::JsStaticMemberExpression;

    #[test]
    fn needs_parentheses() {
        assert_needs_parentheses!("new (test().a)()", JsStaticMemberExpression);
        assert_needs_parentheses!("new (test()[a].b)()", JsStaticMemberExpression);
        assert_needs_parentheses!("new (test()`template`.length)()", JsStaticMemberExpression);
        assert_needs_parentheses!("new (test()!.member)()", JsStaticMemberExpression);

        assert_needs_parentheses!("new (foo?.bar)();", JsStaticMemberExpression);

        assert_not_needs_parentheses!("new (test.a)()", JsStaticMemberExpression);
    }
}
