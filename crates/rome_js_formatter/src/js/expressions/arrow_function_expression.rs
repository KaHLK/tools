use crate::prelude::*;
use rome_formatter::{format_args, write};

use crate::parentheses::{
    is_binary_like_left_or_right, is_conditional_test, is_in_left_hand_side_position,
    NeedsParentheses,
};
use crate::utils::{
    is_simple_expression, resolve_expression, resolve_left_most_expression,
    JsAnyBinaryLikeLeftExpression,
};
use rome_js_syntax::{
    JsAnyArrowFunctionParameters, JsAnyExpression, JsAnyFunctionBody, JsArrowFunctionExpression,
    JsArrowFunctionExpressionFields, JsSyntaxKind, JsSyntaxNode,
};

#[derive(Debug, Clone, Default)]
pub struct FormatJsArrowFunctionExpression;

impl FormatNodeRule<JsArrowFunctionExpression> for FormatJsArrowFunctionExpression {
    fn fmt_fields(
        &self,
        node: &JsArrowFunctionExpression,
        f: &mut JsFormatter,
    ) -> FormatResult<()> {
        use self::JsAnyExpression::*;
        use JsAnyFunctionBody::*;

        let JsArrowFunctionExpressionFields {
            async_token,
            type_parameters,
            parameters,
            return_type_annotation,
            fat_arrow_token,
            body,
        } = node.as_fields();

        if let Some(async_token) = async_token {
            write!(f, [async_token.format(), space()])?;
        }

        write!(f, [type_parameters.format()])?;

        match parameters? {
            JsAnyArrowFunctionParameters::JsAnyBinding(binding) => write!(
                f,
                [format_parenthesize(
                    binding.syntax().first_token().as_ref(),
                    &format_args![binding.format(), if_group_breaks(&text(",")),],
                    binding.syntax().last_token().as_ref(),
                )
                .grouped_with_soft_block_indent()]
            )?,
            JsAnyArrowFunctionParameters::JsParameters(params) => {
                write![f, [group(&params.format())]]?
            }
        }

        write![
            f,
            [
                return_type_annotation.format(),
                space(),
                fat_arrow_token.format(),
                space()
            ]
        ]?;

        let body = body?;

        // With arrays, arrow selfs and objects, they have a natural line breaking strategy:
        // Arrays and objects become blocks:
        //
        //    [
        //      100000,
        //      200000,
        //      300000
        //    ]
        //
        // Arrow selfs get line broken after the `=>`:
        //
        //  (foo) => (bar) =>
        //     (foo + bar) * (foo + bar)
        //
        // Therefore if our body is an arrow self, array, or object, we
        // do not have a soft line break after the arrow because the body is
        // going to get broken anyways.
        let body_has_soft_line_break = match &body {
            JsFunctionBody(_) => true,
            JsAnyExpression(expr) => match expr {
                JsArrowFunctionExpression(_)
                | JsArrayExpression(_)
                | JsObjectExpression(_)
                | JsParenthesizedExpression(_)
                | JsxTagExpression(_) => true,
                expr => is_simple_expression(expr)?,
            },
        };

        // Add parentheses to avoid confusion between `a => b ? c : d` and `a <= b ? c : d`
        // but only if the body isn't an object/function or class expression because parentheses are always required in that
        // case and added by the object expression itself
        let should_add_parens = match &body {
            JsAnyExpression(expression) => {
                let resolved = resolve_expression(expression.clone());

                let is_conditional = matches!(resolved, JsConditionalExpression(_));
                let are_parentheses_mandatory = matches!(
                    resolve_left_most_expression(expression),
                    JsAnyBinaryLikeLeftExpression::JsAnyExpression(
                        JsObjectExpression(_) | JsFunctionExpression(_) | JsClassExpression(_)
                    )
                );

                is_conditional && !are_parentheses_mandatory
            }
            _ => false,
        };

        if body_has_soft_line_break && !should_add_parens {
            write![f, [body.format()]]
        } else {
            write!(
                f,
                [group(&soft_line_indent_or_space(&format_with(|f| {
                    if should_add_parens {
                        write!(f, [if_group_fits_on_line(&text("("))])?;
                    }

                    write!(f, [body.format()])?;

                    if should_add_parens {
                        write!(f, [if_group_fits_on_line(&text(")"))])?;
                    }

                    Ok(())
                })))]
            )
        }
    }

    fn needs_parentheses(&self, item: &JsArrowFunctionExpression) -> bool {
        item.needs_parentheses()
    }
}

impl NeedsParentheses for JsArrowFunctionExpression {
    fn needs_parentheses_with_parent(&self, parent: &JsSyntaxNode) -> bool {
        match parent.kind() {
            JsSyntaxKind::TS_AS_EXPRESSION
            | JsSyntaxKind::JS_UNARY_EXPRESSION
            | JsSyntaxKind::JS_AWAIT_EXPRESSION
            | JsSyntaxKind::TS_TYPE_ASSERTION_EXPRESSION => true,

            _ => {
                is_conditional_test(self.syntax(), parent)
                    || is_in_left_hand_side_position(self.syntax(), parent)
                    || is_binary_like_left_or_right(self.syntax(), parent)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parentheses::NeedsParentheses;
    use crate::{assert_needs_parentheses, assert_not_needs_parentheses};
    use rome_js_syntax::{JsArrowFunctionExpression, SourceType};

    #[test]
    fn needs_parentheses() {
        assert_needs_parentheses!("new (a => test)()`", JsArrowFunctionExpression);
        assert_needs_parentheses!("(a => test)()", JsArrowFunctionExpression);
        assert_needs_parentheses!("(a => test).member", JsArrowFunctionExpression);
        assert_needs_parentheses!("(a => test)[member]", JsArrowFunctionExpression);
        assert_not_needs_parentheses!("object[a => a]", JsArrowFunctionExpression);
        assert_needs_parentheses!("(a => a) as Function", JsArrowFunctionExpression);
        assert_needs_parentheses!("(a => a)!", JsArrowFunctionExpression);
        assert_needs_parentheses!("(a => a)`template`", JsArrowFunctionExpression);
        assert_needs_parentheses!("+(a => a)", JsArrowFunctionExpression);
        assert_needs_parentheses!("(a => a) && b", JsArrowFunctionExpression);
        assert_needs_parentheses!("(a => a) instanceof b", JsArrowFunctionExpression);
        assert_needs_parentheses!("(a => a) in b", JsArrowFunctionExpression);
        assert_needs_parentheses!("(a => a) + b", JsArrowFunctionExpression);
        assert_needs_parentheses!("await (a => a)", JsArrowFunctionExpression);
        assert_needs_parentheses!(
            "<Function>(a => a)",
            JsArrowFunctionExpression,
            SourceType::ts()
        );
        assert_needs_parentheses!("(a => a) ? b : c", JsArrowFunctionExpression);
        assert_not_needs_parentheses!("a ? b => b : c", JsArrowFunctionExpression);
        assert_not_needs_parentheses!("a ? b : c => c", JsArrowFunctionExpression);
        assert_needs_parentheses!("class Test extends (a => a) {}", JsArrowFunctionExpression);
    }
}
