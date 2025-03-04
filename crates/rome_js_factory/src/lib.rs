use crate::generated::JsSyntaxFactory;
use rome_js_syntax::JsLanguage;
use rome_rowan::TreeBuilder;

mod generated;
pub mod make;

// Re-exported for tests
#[doc(hidden)]
pub use rome_js_syntax as syntax;

pub type JsSyntaxTreeBuilder = TreeBuilder<'static, JsLanguage, JsSyntaxFactory>;
