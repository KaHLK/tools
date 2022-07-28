use crate::prelude::*;
use crate::utils::{FormatWithSemicolon, JsAnyBinaryLikeExpression};

use rome_formatter::{format_args, write};
use rome_js_syntax::{
    JsAnyExpression, JsReturnStatement, JsReturnStatementFields, JsSequenceExpression, JsSyntaxKind,
};

#[derive(Debug, Clone, Default)]
pub struct FormatJsReturnStatement;

impl FormatNodeRule<JsReturnStatement> for FormatJsReturnStatement {
    fn fmt_fields(&self, node: &JsReturnStatement, f: &mut JsFormatter) -> FormatResult<()> {
        let JsReturnStatementFields {
            return_token,
            argument,
            semicolon_token,
        } = node.as_fields();

        let format_content = format_with(|f| {
            write!(f, [return_token.format()])?;

            if let Some(argument) = &argument {
                write!(f, [space_token()])?;

                if JsSequenceExpression::can_cast(argument.syntax().kind())
                    || JsAnyBinaryLikeExpression::can_cast(argument.syntax().kind())
                {
                    write!(
                        f,
                        [group_elements(&format_args![
                            if_group_breaks(&token("(")),
                            soft_block_indent(&argument.format()),
                            if_group_breaks(&token(")"))
                        ])]
                    )?;
                } else {
                    write![f, [argument.format()]]?;
                }
            }

            Ok(())
        });

        write!(
            f,
            [FormatWithSemicolon::new(
                &format_content,
                semicolon_token.as_ref()
            )]
        )
    }
}
