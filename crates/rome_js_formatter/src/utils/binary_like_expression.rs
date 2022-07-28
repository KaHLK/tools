use crate::prelude::*;
use rome_formatter::{format_args, write, Buffer, CstFormatContext};
use rome_js_syntax::{
    JsAnyExpression, JsAnyInProperty, JsBinaryExpression, JsBinaryOperator, JsCallExpression,
    JsInExpression, JsInstanceofExpression, JsLogicalExpression, JsLogicalOperator,
    JsParenthesizedExpression, JsPrivateName, JsStaticMemberExpression, JsSyntaxKind, JsSyntaxNode,
    JsSyntaxToken, JsUnaryExpression, OperatorPrecedence,
};

use crate::utils::should_break_after_operator;

use rome_rowan::{declare_node_union, AstNode, SyntaxNode, SyntaxResult};
use std::cmp::Ordering;
use std::fmt::Debug;
use std::iter::FusedIterator;
use std::ops::Deref;

// This function is charge to flat binaryish expressions that have the same precedence of their operators
//
// This means that expressions like `some && thing && elsewhere` are entitled to fall in the same group.
//
// Instead, if we encounter something like `some && thing  || elsewhere && thing`, we will creat two groups:
// `[some, thing]` and `[elsewhere, thing]`, each group will be grouped together.
//
//
// Let's take for example:
//
// ```js
// some && thing && elsewhere && happy
// ```
//
// These expressions have nested nodes, which is roughly something like this:
//
// ```block
// JsLogicalExpression {
//     left: JsLogicalExpression {
//         left: JsLogicalExpression {
//             left: "some"
//             operator: "&&",
//             right: "thing"
//         }
//         operator: "&&"
//         right: "elsewhere"
//     }
//     operator: "&&"
//     right: "happy"
// }
// ```
//
// Our final result should be something like this:
// ```js
// some &&
// thing &&
// elsewhere &&
// happy
// ```
//
// So what we are going to do here is:
// - create a vector of flatten items, where the most nested node is the first one,`left: "some"` in our
// example. The last one will be the first that we encounter, in this case the node that contains `right: "happy"`
// - each element of the vector will contain two elements. One is the AST node, the other one is its
// formatted version
// - the formatted elements will be grouped
//
//
// The flattening of the groups is done by traversing the binary like expression in post-order, first visiting the left most binary like expression:
// - not printing nodes/token twice
// - not "forget" tokens/nodes
// - apply recursions as long as we encounter the same operator
//
// By looking at the formatting, we want to make sure that the operator is always attached to the
// "left" part of the expression, which means that the last "right" wont' have any operator.
//
// In order to achieve that, we basically carry with us the operator of the previous node.
//
// Let's try to understand it by checking the example again. The first time we attempt to create a
// flatten item is when we encounter: `some && thing`, which is a `JsLogicalExpression`.
// Nothing fancy here. Although, if we needed to format this node, you would notice that we don't have
// a second operator, because our end result should be:
//
// ```js
// some &&
// thing &&
// ```
//
// So what we do is to "borrow" (no Rust reference) the operator "&&" that belongs to the "parent" -
// or, if want to see it from a recursion point of view, the previous node that we visited -
// in our case `elsewhere &&`. We then take its operator token and pass it down.
//
// Eventually we will have a `[ JsLogicalExpression, operator2: "&&" ]`.
//
// With these elements, we can now create two formatted elements:
// - `[left, operator: "&&" ]`
// - `[right, operator2: "&&" ]`
//
// Now let's continue until we arrive to the last node that we want to try to format, which is:
// `&& happy`. If we follow the logic explained so far, this node doesn't have an operator
// anymore because we passed it to its child. And we can't try to add a new operator.
// But this is fine! Because this is want we wanted! By removing the operator, we are left with `happy`
// which is what we wanted since the beginning!

/// Small wrapper to identify the operation of an expression and deduce their precedence
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum BinaryLikeOperator {
    Logical(JsLogicalOperator),
    Binary(JsBinaryOperator),
    Instanceof,
    In,
}

impl BinaryLikeOperator {
    fn precedence(&self) -> OperatorPrecedence {
        match self {
            BinaryLikeOperator::Logical(op) => op.precedence(),
            BinaryLikeOperator::Binary(op) => op.precedence(),
            BinaryLikeOperator::Instanceof => OperatorPrecedence::Relational,
            BinaryLikeOperator::In => OperatorPrecedence::Relational,
        }
    }
}

//     /// Flattens the right hand operand of a binary like expression.
//     fn flatten_right_hand_side(
//         &mut self,
//         binary_like_expression: JsAnyBinaryLikeExpression,
//         parent_operator: Option<JsSyntaxToken>,
//     ) -> FormatResult<()> {
//         let right = JsAnyBinaryLikeLeftExpression::JsAnyExpression(binary_like_expression.right()?);
//         let has_comments = right.syntax().has_comments_direct();
//
//         let flatten_item = FlattenItem::new(
//             FlattenedBinaryExpressionPart::Right {
//                 parent: binary_like_expression,
//             },
//             parent_operator,
//             has_comments.into(),
//         );
//         self.items.push(flatten_item);
//
//         Ok(())
//     }
//
//     /// The left hand-side expression and the current operator cannot be flattened.
//     /// Format the left hand side on its own and potentially wrap it in parentheses before formatting
//     /// the right-hand side of the current expression.
//     fn flatten_new_binary_like_group(
//         &mut self,
//         binary_like_expression: JsAnyBinaryLikeExpression,
//         parent_operator: Option<JsSyntaxToken>,
//     ) -> FormatResult<()> {
//         if let Some(last) = self.items.last_mut() {
//             // Remove any line breaks and the trailing operator so that the operator/trailing aren't part
//             // of the parenthesized expression.
//             last.terminator = TrailingTerminator::None;
//             last.operator = None;
//         }
//
//         let left = binary_like_expression.left()?;
//         let operator = binary_like_expression.operator()?;
//         let operator_token = binary_like_expression.operator_token()?;
//
//         let operator_has_trailing_comments = operator_token.has_trailing_comments();
//         let left_parenthesized = needs_parens(operator, &left)?;
//         let mut left_item = FlattenItem::new(
//             FlattenedBinaryExpressionPart::Group {
//                 current: left,
//                 expressions_start: self.current_group_start,
//                 expressions_end: self.items.len(),
//                 parenthesized: left_parenthesized,
//             },
//             Some(operator_token),
//             operator_has_trailing_comments.into(),
//         );
//
//         if operator_has_trailing_comments {
//             left_item = left_item.with_terminator(TrailingTerminator::HardLineBreak);
//         }
//
//         self.current_group_start = self.len();
//         self.items.push(left_item);
//
//         let right = JsAnyBinaryLikeLeftExpression::JsAnyExpression(binary_like_expression.right()?);
//
//         // Flatten the right node
//         let parent_operator_has_comments = parent_operator
//             .as_ref()
//             .map(|operator| operator.has_leading_comments());
//
//         let mut right_item = FlattenItem::new(
//             FlattenedBinaryExpressionPart::Right {
//                 parent: binary_like_expression,
//             },
//             parent_operator,
//             Commented::No,
//         );
//
//         // Format the parent operator
//         if let Some(parent_operator_has_comments) = parent_operator_has_comments {
//             // Here we care only about trailing comments that belong to the previous operator
//             if parent_operator_has_comments {
//                 right_item = right_item
//                     .with_comments(true)
//                     .with_terminator(TrailingTerminator::HardLineBreak)
//             }
//         } else {
//             // Here we want to check only leading comments;
//             // trailing comments will be added after the end of the whole expression.
//             // We want to handle cases like `lorem && (3 + 5 == 9) // comment`.
//             // This part is a signal to the formatter to tell it if the whole expression should break.
//             right_item = right_item.with_comments(right.syntax().has_leading_comments())
//         };
//
//         self.items.push(right_item);
//
//         Ok(())
//     }
// }

declare_node_union! {
    pub(crate) JsAnyBinaryLikeExpression = JsLogicalExpression | JsBinaryExpression | JsInstanceofExpression | JsInExpression
}

impl Format<JsFormatContext> for JsAnyBinaryLikeExpression {
    fn fmt(&self, f: &mut Formatter<JsFormatContext>) -> FormatResult<()> {
        let is_inside_parenthesis = self.is_inside_parenthesis();

        let format_content = format_with(|f| {
            let mut parts = split_expression_into_parts(self, is_inside_parenthesis, f)?;
            let last_is_jsx = parts.last().map_or(false, |part| part.is_jsx());

            if is_inside_parenthesis {
                dbg!("inside parenthesis");
                return write!(f, [&format_once(|f| { f.join().entries(parts).finish() })]);
            }

            if let Some(parent) = self.syntax().parent() {
                if let Some(call_expression) = JsCallExpression::cast(parent.clone()) {
                    if call_expression.callee().as_ref().map(|node| node.syntax())
                        == Ok(self.syntax())
                    {
                        return write!(
                            f,
                            [group_elements(&soft_block_indent(&format_once(|f| {
                                f.join().entries(parts).finish()
                            })))]
                        );
                    }
                } else if JsUnaryExpression::can_cast(parent.kind())
                    || JsStaticMemberExpression::can_cast(parent.kind())
                {
                    return write!(
                        f,
                        [group_elements(&soft_block_indent(&format_once(|f| {
                            f.join().entries(parts).finish()
                        })))]
                    );
                }
            }

            let should_not_indent = should_not_indent_if_parent_indents(self);
            let inlines = self.should_inline_logical_expression()?;

            dbg!(should_not_indent, inlines);

            // FIXME: How deal with parenthesized expressions
            // and make sure that if this is a JSX element that it will always go through the JSX part?
            // Or is there a counterpart in FormatJsxElement
            if !last_is_jsx
                && (should_not_indent || (!inlines && should_indent_if_parent_inlines(self)))
            {
                dbg!("not indent");
                return write!(
                    f,
                    [group_elements(&format_once(|f| {
                        f.join().entries(parts).finish()
                    }))]
                );
            }

            if let Some(first) = parts.first() {
                dbg!("default", &first);
                // if none of the previous conditions is met,
                // we take out the first element from the rest of the group
                // and indent the rest of the groups in a new line
                dbg!(last_is_jsx);

                let tail_parts = if last_is_jsx {
                    &parts[1..parts.len() - 1]
                } else {
                    &parts[1..]
                };

                dbg!(tail_parts);

                let group_id = f.group_id("logicalChain");

                let format_non_jsx_parts = format_with(|f| {
                    write!(
                        f,
                        [group_elements(&format_args![
                            first,
                            indent(&format_once(|f| {
                                f.join().entries(tail_parts.iter()).finish()
                            }))
                        ])
                        .with_group_id(Some(group_id))]
                    )
                });

                if last_is_jsx {
                    // SAFETY: `last_is_jsx` is only true if parts is not empty
                    let jsx_element = parts.last().unwrap().memoized();
                    write!(
                        f,
                        [group_elements(&format_args![
                            format_non_jsx_parts,
                            if_group_breaks(&block_indent(&jsx_element))
                                .with_group_id(Some(group_id)),
                            if_group_fits_on_line(&jsx_element).with_group_id(Some(group_id))
                        ])]
                    )
                } else {
                    write!(f, [format_non_jsx_parts])
                }
            } else {
                // Empty (unlikely)
                Ok(())
            }
        });

        if let Some(parent_binary) = self.parent::<JsAnyBinaryLikeExpression>() {
            if binary_needs_parens(&parent_binary, &self)? {
                return write!(
                    f,
                    [format_parenthesize(
                        self.syntax().first_token(),
                        &format_content,
                        self.syntax().last_token()
                    )]
                );
            }
        }

        write!(f, [format_content])
    }
}

fn split_expression_into_parts(
    root: &JsAnyBinaryLikeExpression,
    inside_parenthesis: bool,
    f: &mut JsFormatter,
) -> SyntaxResult<Vec<BinaryExpressionPart>> {
    // Stores the left and right parts of the binary expression in sequence (rather than nested as they
    // appear in the tree).
    let mut items = Vec::new();

    let mut expressions = InOrderIterator::new(root.clone());

    let mut parent_operator: Option<BinaryLikeOperator> = None;

    while let Some(event) = expressions.next() {
        match event {
            VisitEvent::Enter(expression) => {
                if !expression.can_flatten()? {
                    // Stop at this expression. This is either not a binary expression OR it has
                    // different precedence and needs to be grouped separately.
                    expressions.skip_subtree();

                    items.push(BinaryExpressionPart::Left {
                        node: JsAnyBinaryLikeLeftExpression::from(expression.left()?),
                    });

                    parent_operator = Some(expression.operator()?);
                }
            }
            VisitEvent::Exit(expression) => {
                // It's only possible to suppress the formatting of the whole binary expression formatting OR
                // the formatting of the right hand side value but not of a nested binary expression.
                f.context()
                    .comments()
                    .mark_suppression_checked(expression.syntax());

                items.push(BinaryExpressionPart::Right {
                    parent: expression,
                    inside_parenthesis,
                })
            }
        }
    }

    Ok(items)
}

#[derive(Debug, Clone)]
enum BinaryExpressionPart {
    /// A terminal left hand side of a binary expression
    Left { node: JsAnyBinaryLikeLeftExpression },

    /// The right hand side of a binary expression together with its operand
    Right {
        parent: JsAnyBinaryLikeExpression,
        /// Is this binary expression group wrapped inside parenthesis. For example, because
        /// it is inside of an if statements condition?
        inside_parenthesis: bool,
    },
}

impl BinaryExpressionPart {
    fn is_jsx(&self) -> bool {
        match self {
            BinaryExpressionPart::Left { node } => match node {
                JsAnyBinaryLikeLeftExpression::JsAnyExpression(expression) => matches!(
                    expression.skip_parens(),
                    Ok(JsAnyExpression::JsxTagExpression(_))
                ),
                _ => false,
            },
            BinaryExpressionPart::Right { parent, .. } => {
                if let Ok(right) = parent.right().and_then(|right| right.skip_parens()) {
                    matches!(right, JsAnyExpression::JsxTagExpression(_))
                } else {
                    false
                }
            }
        }
    }
}

impl Format<JsFormatContext> for BinaryExpressionPart {
    fn fmt(&self, f: &mut Formatter<JsFormatContext>) -> FormatResult<()> {
        match self {
            BinaryExpressionPart::Left { node } => {
                write!(f, [group_elements(&node)])
            }
            BinaryExpressionPart::Right {
                parent: binary_like_expression,
                inside_parenthesis,
            } => {
                // TODO COMMENTS

                write!(
                    f,
                    [
                        space_token(),
                        binary_like_expression.operator_token().format(),
                    ]
                )?;

                let right = binary_like_expression.right()?;
                let operator_and_right_expression = format_with(|f| {
                    let should_inline =
                        binary_like_expression.should_inline_logical_expression()?;
                    if should_inline {
                        write!(f, [space_token()])?;
                    } else {
                        write!(f, [soft_line_break_or_space()])?;
                    }

                    write!(f, [right.format()])
                });

                let syntax = binary_like_expression.syntax();
                let parent_has_same_kind =
                    Some(syntax.kind()) == syntax.parent().map(|node| node.kind());
                let left_has_same_kind =
                    binary_like_expression.left()?.syntax().kind() == syntax.kind();
                let right_has_same_kind = right.syntax().kind() == syntax.kind();

                // TODO comments
                let should_group = !parent_has_same_kind
                    && !left_has_same_kind
                    && !right_has_same_kind
                    && !(*inside_parenthesis
                        && matches!(
                            binary_like_expression,
                            JsAnyBinaryLikeExpression::JsLogicalExpression(_)
                        ));

                if dbg!(should_group) {
                    write!(f, [group_elements(&operator_and_right_expression)])
                } else {
                    write!(f, [operator_and_right_expression])
                }
            }
        }
    }
}

impl JsAnyBinaryLikeExpression {
    fn left(&self) -> SyntaxResult<JsAnyBinaryLikeLeftExpression> {
        match self {
            JsAnyBinaryLikeExpression::JsLogicalExpression(logical) => logical
                .left()
                .map(JsAnyBinaryLikeLeftExpression::JsAnyExpression),
            JsAnyBinaryLikeExpression::JsBinaryExpression(binary) => binary
                .left()
                .map(JsAnyBinaryLikeLeftExpression::JsAnyExpression),
            JsAnyBinaryLikeExpression::JsInstanceofExpression(instanceof) => instanceof
                .left()
                .map(JsAnyBinaryLikeLeftExpression::JsAnyExpression),
            JsAnyBinaryLikeExpression::JsInExpression(in_expression) => in_expression
                .property()
                .map(JsAnyBinaryLikeLeftExpression::from),
        }
    }

    fn into_expression(self) -> JsAnyExpression {
        match self {
            JsAnyBinaryLikeExpression::JsLogicalExpression(logical) => {
                JsAnyExpression::JsLogicalExpression(logical)
            }
            JsAnyBinaryLikeExpression::JsBinaryExpression(binary) => {
                JsAnyExpression::JsBinaryExpression(binary)
            }
            JsAnyBinaryLikeExpression::JsInstanceofExpression(instance) => {
                JsAnyExpression::JsInstanceofExpression(instance)
            }
            JsAnyBinaryLikeExpression::JsInExpression(in_expression) => {
                JsAnyExpression::JsInExpression(in_expression)
            }
        }
    }

    fn operator_token(&self) -> SyntaxResult<JsSyntaxToken> {
        match self {
            JsAnyBinaryLikeExpression::JsLogicalExpression(logical) => logical.operator_token(),
            JsAnyBinaryLikeExpression::JsBinaryExpression(binary) => binary.operator_token(),
            JsAnyBinaryLikeExpression::JsInstanceofExpression(instanceof) => {
                instanceof.instanceof_token()
            }
            JsAnyBinaryLikeExpression::JsInExpression(in_expression) => in_expression.in_token(),
        }
    }

    pub(crate) fn operator(&self) -> SyntaxResult<BinaryLikeOperator> {
        match self {
            JsAnyBinaryLikeExpression::JsLogicalExpression(logical) => {
                logical.operator().map(BinaryLikeOperator::Logical)
            }
            JsAnyBinaryLikeExpression::JsBinaryExpression(binary) => {
                binary.operator().map(BinaryLikeOperator::Binary)
            }
            JsAnyBinaryLikeExpression::JsInstanceofExpression(_) => {
                Ok(BinaryLikeOperator::Instanceof)
            }
            JsAnyBinaryLikeExpression::JsInExpression(_) => Ok(BinaryLikeOperator::In),
        }
    }

    fn right(&self) -> SyntaxResult<JsAnyExpression> {
        match self {
            JsAnyBinaryLikeExpression::JsLogicalExpression(logical) => logical.right(),
            JsAnyBinaryLikeExpression::JsBinaryExpression(binary) => binary.right(),
            JsAnyBinaryLikeExpression::JsInstanceofExpression(instanceof) => instanceof.right(),
            JsAnyBinaryLikeExpression::JsInExpression(in_expression) => in_expression.object(),
        }
    }

    // Break the children if the expression is inside of e.g. an if statement's condition
    fn is_inside_parenthesis(&self) -> bool {
        if let Some(parent) = self.syntax().parent() {
            matches!(
                parent.kind(),
                JsSyntaxKind::JS_IF_STATEMENT
                    | JsSyntaxKind::JS_DO_WHILE_STATEMENT
                    | JsSyntaxKind::JS_WHILE_STATEMENT
                    | JsSyntaxKind::JS_SWITCH_STATEMENT
                    | JsSyntaxKind::JS_TEMPLATE_ELEMENT
                    | JsSyntaxKind::TS_TEMPLATE_ELEMENT
            )
        } else {
            false
        }
    }

    /// Determines if a binary like expression should be flattened or not. As a rule of thumb, an expression
    /// can be flattened if its left hand side has the same operator-precedence
    fn can_flatten(&self) -> SyntaxResult<bool> {
        if let Some(left_binary_like) = JsAnyBinaryLikeExpression::cast(self.left()?.into_syntax())
        {
            let operator = self.operator()?;
            let left_operator = left_binary_like.operator()?;

            Ok(should_flatten(operator, left_operator))
        } else {
            Ok(false)
        }
    }

    pub(crate) fn should_inline_logical_expression(&self) -> SyntaxResult<bool> {
        let inline = if let JsAnyBinaryLikeExpression::JsLogicalExpression(expression) = self {
            match expression.right()? {
                JsAnyExpression::JsObjectExpression(object) => !object.members().is_empty(),
                JsAnyExpression::JsArrayExpression(array) => !array.elements().is_empty(),
                JsAnyExpression::JsxTagExpression(_) => true,
                _ => false,
            }
        } else {
            false
        };

        Ok(inline)
    }
}

declare_node_union! {
    JsAnyBinaryLikeLeftExpression = JsAnyExpression | JsPrivateName
}

impl JsAnyBinaryLikeLeftExpression {
    fn as_expression(&self) -> Option<&JsAnyExpression> {
        match self {
            JsAnyBinaryLikeLeftExpression::JsAnyExpression(expression) => Some(expression),
            JsAnyBinaryLikeLeftExpression::JsPrivateName(_) => None,
        }
    }
}

impl Format<JsFormatContext> for JsAnyBinaryLikeLeftExpression {
    fn fmt(&self, f: &mut JsFormatter) -> FormatResult<()> {
        match self {
            JsAnyBinaryLikeLeftExpression::JsAnyExpression(expression) => {
                write![f, [expression.format()]]
            }
            JsAnyBinaryLikeLeftExpression::JsPrivateName(private_name) => {
                write![f, [private_name.format()]]
            }
        }
    }
}

impl From<JsAnyInProperty> for JsAnyBinaryLikeLeftExpression {
    fn from(property: JsAnyInProperty) -> Self {
        match property {
            JsAnyInProperty::JsAnyExpression(expression) => {
                JsAnyBinaryLikeLeftExpression::JsAnyExpression(expression)
            }
            JsAnyInProperty::JsPrivateName(private_name) => {
                JsAnyBinaryLikeLeftExpression::JsPrivateName(private_name)
            }
        }
    }
}

/// This function is in charge of formatting a node inside a binaryish expression with parenthesis or not
///
/// At the moment this logic is applied only to logical expressions.
///
/// A logical expressions should be decorated with parenthesis only if its previous operation has a lower
/// precedence.
///
/// For example:
///
/// ```ignore
/// foo && bar || lorem
/// ```
///
/// The logical expression `foo && bar` has higher precedence of `bar || lorem`. This means that
/// first `foo && bar` is computed and its result is then computed against `|| lorem`.
///
/// In order to make this distinction more obvious, we wrap `foo && bar` in parenthesis.
pub(crate) fn binary_needs_parens(
    parent: &JsAnyBinaryLikeExpression,
    node: &JsAnyBinaryLikeExpression,
) -> SyntaxResult<bool> {
    let parent_operator = parent.operator()?;

    let is_right = parent.right()?.syntax() == node.syntax();

    let result = match node {
        JsAnyBinaryLikeExpression::JsLogicalExpression(logical) => {
            // `a && b || c` -> `(a && b) || c`
            parent_operator != BinaryLikeOperator::Logical(logical.operator()?)
        }
        JsAnyBinaryLikeExpression::JsBinaryExpression(binary) => {
            let operator = binary.operator()?;
            let operator_precedence = operator.precedence();
            let parent_precedence = parent_operator.precedence();

            if parent_precedence > operator_precedence {
                true
            }
            // Doesn't apply if the expressions use the same operands because the formatting
            // groups all these expressions together.
            // `4 * 3 / 2` => `(4 * 3) / 2`
            else if is_right && parent_precedence == operator_precedence {
                true
            } else if parent_precedence == operator_precedence
                && !should_flatten(parent_operator, BinaryLikeOperator::Binary(operator))
            {
                true
            }
            // Not mandatory but adds parentheses around child expressions if the parent expression uses bitshift operations
            // `4 + 3 << 2` -> `(4 + 3) << 2`
            else if matches!(
                parent_precedence,
                OperatorPrecedence::BitwiseAnd
                    | OperatorPrecedence::BitwiseOr
                    | OperatorPrecedence::BitwiseXor
                    | OperatorPrecedence::Shift
            ) {
                true
            }
            // Add parens for `4 % 3 + 4` -> `(4 % 3) + 4`
            else if matches!(
                parent_operator,
                BinaryLikeOperator::Binary(JsBinaryOperator::Plus | JsBinaryOperator::Minus)
            ) && operator == JsBinaryOperator::Remainder
            {
                true
            } else {
                false
            }
        }
        JsAnyBinaryLikeExpression::JsInstanceofExpression(_) => {
            // `instanceof` operator has higher precedence than `in` operator, so we apply parenthesis here
            matches!(parent_operator, BinaryLikeOperator::In)
        }
        _ => false,
    };

    Ok(result)
}

fn should_flatten(parent_operator: BinaryLikeOperator, operator: BinaryLikeOperator) -> bool {
    if operator.precedence() != parent_operator.precedence() {
        return false;
    }

    match (parent_operator.precedence(), operator.precedence()) {
        (OperatorPrecedence::Equality, OperatorPrecedence::Equality) => false,
        (OperatorPrecedence::Multiplicative, OperatorPrecedence::Multiplicative) => {
            if parent_operator == BinaryLikeOperator::Binary(JsBinaryOperator::Remainder)
                || operator == BinaryLikeOperator::Binary(JsBinaryOperator::Remainder)
            {
                false
            } else if parent_operator != operator {
                false
            } else {
                true
            }
        }
        (OperatorPrecedence::Shift, OperatorPrecedence::Shift) => false,
        _ => true,
    }
}

/// This function checks whether the chain of logical/binary expressions **should not** be indented
///
/// There are some cases where the indentation is done by the parent, so if the parent is already doing
/// the indentation, then there's no need to do a second indentation.
/// [Prettier applies]: https://github.com/prettier/prettier/blob/b0201e01ef99db799eb3716f15b7dfedb0a2e62b/src/language-js/print/binaryish.js#L122-L125
fn should_not_indent_if_parent_indents(current_node: &JsAnyBinaryLikeExpression) -> bool {
    match current_node.syntax().parent() {
        None => false,
        Some(parent) => {
            let great_parent_kind = parent.parent().map(|node| node.kind());

            match (parent.kind(), great_parent_kind) {
                (JsSyntaxKind::JS_PROPERTY_OBJECT_MEMBER, _)
                | (
                    JsSyntaxKind::JS_INITIALIZER_CLAUSE,
                    Some(JsSyntaxKind::JS_VARIABLE_DECLARATOR),
                ) => should_break_after_operator(&current_node.clone().into_expression())
                    .unwrap_or(false),
                (
                    JsSyntaxKind::JS_RETURN_STATEMENT
                    | JsSyntaxKind::JS_THROW_STATEMENT
                    | JsSyntaxKind::JS_TEMPLATE
                    | JsSyntaxKind::JS_ARROW_FUNCTION_EXPRESSION,
                    _,
                ) => true,
                _ => false,
            }
        }
    }
}

/// There are other cases where the parent decides to inline the the element; in
/// these cases the decide to actually break on a new line and indent it.
///
/// This function checks what the parents adheres to this behaviour
fn should_indent_if_parent_inlines(current_node: &JsAnyBinaryLikeExpression) -> bool {
    fn match_parent(parent: JsSyntaxNode) -> bool {
        match parent.kind() {
            JsSyntaxKind::JS_ASSIGNMENT_EXPRESSION
            | JsSyntaxKind::JS_PROPERTY_CLASS_MEMBER
            | JsSyntaxKind::TS_PROPERTY_SIGNATURE_CLASS_MEMBER
            | JsSyntaxKind::JS_PROPERTY_OBJECT_MEMBER => true,

            JsSyntaxKind::JS_INITIALIZER_CLAUSE => {
                if let Some(grand_parent) = parent.parent() {
                    matches!(grand_parent.kind(), JsSyntaxKind::JS_VARIABLE_DECLARATOR)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    let mut current = current_node.syntax().clone();

    // FIXME is the while loop necessary here?
    while let Some(parent) = current.parent() {
        if JsParenthesizedExpression::can_cast(parent.kind()) {
            current = parent
        } else {
            return match_parent(parent);
        }
    }

    false
}

/// The [PostorderIterator] visits every node twice. First on the way down to find the left most binary
/// like expression, then on the way back up when it yields the binary like expressions.
/// This enum encodes the information whatever the iterator is on its way down (`Enter`) or traversing
/// upwards (`Exit`).
#[derive(Debug, Eq, PartialEq, Clone)]
enum VisitEvent {
    Enter(JsAnyBinaryLikeExpression),
    Exit(JsAnyBinaryLikeExpression),
}

/// Iterator that first returns the left-most binary-like expression and then traverses upwards to the start node.
/// The binary like expression nodes are yielded when traversing upwards.
///
/// # Examples
///
/// ```js
/// a && b && c && d
/// ```
/// This produces a tree with the following shape:
///
/// ```txt
///         &&
///        / \
///       /   \
///      &&   d
///     / \
///    /   \
///   &&    c
///  / \
/// a   b
/// ```
///
/// The iterator follows the left branches of the binary expressions without until it hits any non
/// binary-like expression (in this case the reference identifier `a`). From there, the iterator starts
/// traversing upwards again and yields the binary expression along the way. The returned nodes for the above
/// examples are (in that exact order):
/// 1. `a && b`
/// 2. `a && b && c`
/// 3. `a && b && c && d`
struct InOrderIterator {
    /// The next node to visit or [None] if the iterator passed the start node (is at its end).
    next: Option<VisitEvent>,

    /// The start node. Necessary to know when to stop iterating.
    start: JsSyntaxNode,

    skip_subtree: bool,
}

impl InOrderIterator {
    fn new(start: JsAnyBinaryLikeExpression) -> Self {
        Self {
            start: start.syntax().clone(),
            next: Some(VisitEvent::Enter(start)),
            skip_subtree: false,
        }
    }

    fn skip_subtree(&mut self) {
        self.skip_subtree = true;
    }
}

impl Iterator for InOrderIterator {
    type Item = VisitEvent;

    fn next(&mut self) -> Option<Self::Item> {
        if self.skip_subtree {
            self.next = self.next.take().and_then(|next| match next {
                VisitEvent::Enter(node) => {
                    if node.syntax() == &self.start {
                        None
                    } else {
                        // SAFETY: Calling `unwrap` here is safe because the iterator only enters (traverses into) a node
                        // if it is a valid binary like expression and it is guaranteed to have a parent.
                        let expression = node
                            .syntax()
                            .parent()
                            .and_then(JsAnyBinaryLikeExpression::cast)
                            .unwrap();

                        Some(VisitEvent::Exit(expression))
                    }
                }
                VisitEvent::Exit(node) => Some(VisitEvent::Exit(node)),
            });
            self.skip_subtree = false;
        }

        let next = self.next.take()?;
        match &next {
            VisitEvent::Enter(binary) => {
                let left_expression = binary
                    .left()
                    .ok()
                    .and_then(|left| JsAnyBinaryLikeExpression::cast(left.into_syntax()));
                if let Some(expression) = left_expression {
                    self.next = Some(VisitEvent::Enter(expression));
                } else {
                    // If left is missing or it isn't a binary like expression, then format it as part of the parent binary like expression
                    self.next = Some(VisitEvent::Exit(binary.clone()));
                }
            }
            VisitEvent::Exit(node) => {
                if node.syntax() != &self.start {
                    self.next = node.syntax().parent().map(|parent| {
                        // SAFETY: Calling `unwrap` here is safe because the iterator only enters (traverses into) a node
                        // if it is a valid binary like expression.
                        let expression = JsAnyBinaryLikeExpression::cast(parent).unwrap();
                        VisitEvent::Exit(expression)
                    });
                }
            }
        };

        Some(next)
    }
}

impl FusedIterator for InOrderIterator {}

#[cfg(test)]
mod tests {
    use crate::utils::binary_like_expression::{InOrderIterator, VisitEvent};
    use crate::utils::JsAnyBinaryLikeExpression;
    use rome_js_parser::parse_module;
    use rome_js_syntax::{JsBinaryExpression, JsLogicalExpression};
    use rome_rowan::AstNode;

    #[test]
    fn in_order_visits_every_binary_like_expression() {
        let parse = parse_module("a && b && c || d", 0);
        let root = parse
            .syntax()
            .descendants()
            .find_map(JsLogicalExpression::cast)
            .unwrap();
        let a_and_b_and_c = JsLogicalExpression::unwrap_cast(root.left().unwrap().into_syntax());
        let a_and_b = JsLogicalExpression::unwrap_cast(a_and_b_and_c.left().unwrap().into_syntax());

        let mut iterator = InOrderIterator::new(JsAnyBinaryLikeExpression::from(root.clone()));

        assert_eq!(
            iterator.next(),
            Some(VisitEvent::Enter(JsAnyBinaryLikeExpression::from(
                root.clone()
            )))
        );
        assert_eq!(
            iterator.next(),
            Some(VisitEvent::Enter(JsAnyBinaryLikeExpression::from(
                a_and_b_and_c.clone()
            )))
        );
        assert_eq!(
            iterator.next(),
            Some(VisitEvent::Enter(JsAnyBinaryLikeExpression::from(
                a_and_b.clone()
            )))
        );

        assert_eq!(
            iterator.next(),
            Some(VisitEvent::Exit(JsAnyBinaryLikeExpression::from(a_and_b)))
        );
        assert_eq!(
            iterator.next(),
            Some(VisitEvent::Exit(JsAnyBinaryLikeExpression::from(
                a_and_b_and_c
            )))
        );
        assert_eq!(
            iterator.next(),
            Some(VisitEvent::Exit(JsAnyBinaryLikeExpression::from(root)))
        );
    }

    #[test]
    fn in_order_skip_subtree() {
        let parse = parse_module("a && b && c || d", 0);
        let root = parse
            .syntax()
            .descendants()
            .find_map(JsLogicalExpression::cast)
            .unwrap();
        let a_and_b_and_c = JsLogicalExpression::unwrap_cast(root.left().unwrap().into_syntax());

        let mut iterator = InOrderIterator::new(JsAnyBinaryLikeExpression::from(root.clone()));

        assert_eq!(
            iterator.next(),
            Some(VisitEvent::Enter(JsAnyBinaryLikeExpression::from(
                root.clone()
            )))
        );
        assert_eq!(
            iterator.next(),
            Some(VisitEvent::Enter(JsAnyBinaryLikeExpression::from(
                a_and_b_and_c.clone()
            )))
        );

        // skip over a && b
        iterator.skip_subtree();

        assert_eq!(
            iterator.next(),
            Some(VisitEvent::Exit(JsAnyBinaryLikeExpression::from(
                a_and_b_and_c
            )))
        );
        assert_eq!(
            iterator.next(),
            Some(VisitEvent::Exit(JsAnyBinaryLikeExpression::from(root)))
        );
    }
}
