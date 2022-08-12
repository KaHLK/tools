use crate::prelude::*;

use crate::parentheses::{resolve_expression_parent, NeedsParentheses};
use crate::utils::{resolve_expression, MemberChainLabel};
use rome_formatter::{format_args, write};
use rome_js_syntax::{
    JsAnyExpression, JsAssignmentExpression, JsInitializerClause, JsStaticMemberExpression,
    JsStaticMemberExpressionFields, JsSyntaxKind, JsSyntaxNode,
};
use rome_rowan::AstNode;

#[derive(Debug, Clone, Default)]
pub struct FormatJsStaticMemberExpression;

struct MemberLabel;

#[derive(Copy, Clone, Debug)]
enum Label {
    Member,
    MemberChain,
}

impl Label {
    fn label_id(&self) -> LabelId {
        match self {
            Label::Member => LabelId::of::<MemberLabel>(),
            Label::MemberChain => LabelId::of::<MemberChainLabel>(),
        }
    }
}

impl FormatNodeRule<JsStaticMemberExpression> for FormatJsStaticMemberExpression {
    fn fmt_fields(&self, node: &JsStaticMemberExpression, f: &mut JsFormatter) -> FormatResult<()> {
        let JsStaticMemberExpressionFields {
            object,
            operator_token,
            member,
        } = node.as_fields();

        let mut object_label: Option<Label> = None;
        {
            let mut buffer = f.inspect(|element| {
                object_label = if element.has_label(LabelId::of::<MemberLabel>()) {
                    Some(Label::Member)
                } else if element.has_label(LabelId::of::<MemberChainLabel>()) {
                    Some(Label::MemberChain)
                } else {
                    None
                };
            });

            write!(buffer, [object.format()])?;
        }

        let layout = compute_member_layout(node, object_label)?;

        let format_inner = format_with(|f| match layout {
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
        });

        let label = match object_label {
            Some(Label::MemberChain) => Label::MemberChain,
            _ => Label::Member,
        };

        write!(f, [labelled(label.label_id(), &format_inner)])
    }

    fn needs_parentheses(&self, item: &JsStaticMemberExpression) -> bool {
        item.needs_parentheses()
    }
}

#[derive(Debug, Copy, Clone)]
enum StaticMemberExpressionLayout {
    /// Forces that there's no line break between the object, operator, and member
    NoBreak,

    /// Breaks the static member expression after the object if the whole expression doesn't fit on a single line
    BreakAfterObject,
}

fn compute_member_layout(
    member: &JsStaticMemberExpression,
    object_label: Option<Label>,
) -> FormatResult<StaticMemberExpressionLayout> {
    let parent = resolve_expression_parent(member.syntax());

    let nested = parent
        .as_ref()
        .map_or(false, |p| JsStaticMemberExpression::can_cast(p.kind()));

    let object = resolve_expression(member.object()?);

    if let Some(parent) = &parent {
        if JsAssignmentExpression::can_cast(parent.kind())
            || JsInitializerClause::can_cast(parent.kind())
        {
            let no_break = match &object {
                JsAnyExpression::JsCallExpression(call_expression) => {
                    !call_expression.arguments()?.args().is_empty()
                }
                JsAnyExpression::TsNonNullAssertionExpression(non_null_assertion) => {
                    match resolve_expression(non_null_assertion.expression()?) {
                        JsAnyExpression::JsCallExpression(call_expression) => {
                            !call_expression.arguments()?.args().is_empty()
                        }
                        _ => false,
                    }
                }
                _ => matches!(object_label, Some(Label::MemberChain)),
            };

            if no_break {
                return Ok(StaticMemberExpressionLayout::NoBreak);
            }
        }
    };

    if !nested && matches!(object, JsAnyExpression::JsIdentifierExpression(_)) {
        return Ok(StaticMemberExpressionLayout::NoBreak);
    }

    let first_non_static_member_ancestor = member.syntax().ancestors().find(|parent| {
        !matches!(
            parent.kind(),
            JsSyntaxKind::JS_STATIC_MEMBER_EXPRESSION | JsSyntaxKind::JS_PARENTHESIZED_EXPRESSION
        )
    });

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

        member_chain_callee_needs_parens(self.clone().into(), parent)
    }
}

pub(crate) fn member_chain_callee_needs_parens(
    node: JsAnyExpression,
    parent: &JsSyntaxNode,
) -> bool {
    use JsAnyExpression::*;

    match parent.kind() {
        // `new (test().a)
        JsSyntaxKind::JS_NEW_EXPRESSION => {
            let mut object_chain =
                std::iter::successors(Some(node), |expression| match expression {
                    JsStaticMemberExpression(member) => member.object().ok(),
                    JsComputedMemberExpression(member) => member.object().ok(),
                    JsTemplate(template) => template.tag(),
                    TsNonNullAssertionExpression(assertion) => assertion.expression().ok(),
                    JsParenthesizedExpression(expression) => expression.expression().ok(),
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
