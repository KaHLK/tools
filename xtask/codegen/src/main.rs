#[cfg(feature = "configuration")]
mod generate_configuration;
#[cfg(feature = "schema")]
mod generate_schema;

use pico_args::Arguments;
use xtask::{project_root, pushd, Mode, Result};

#[cfg(feature = "configuration")]
use crate::generate_configuration::generate_rules_configuration;
#[cfg(feature = "schema")]
use crate::generate_schema::generate_configuration_schema;
use xtask_codegen::{
    generate_analyzer, generate_ast, generate_formatter, generate_parser_tests, generate_tables,
};

fn main() -> Result<()> {
    let _d = pushd(project_root());

    let mut args = Arguments::from_env();
    let command = args.subcommand()?.unwrap_or_default();
    match command.as_str() {
        "grammar" => {
            generate_grammar(args);
            Ok(())
        }
        "formatter" => {
            generate_formatter();
            Ok(())
        }
        "test" => {
            generate_parser_tests(Mode::Overwrite)?;
            Ok(())
        }
        "unicode" => {
            generate_tables()?;
            Ok(())
        }
        "analyzer" => {
            generate_analyzer()?;
            Ok(())
        }
        #[cfg(feature = "configuration")]
        "configuration" => {
            generate_rules_configuration(Mode::Overwrite)?;
            Ok(())
        }
        #[cfg(feature = "schema")]
        "schema" => {
            generate_configuration_schema(Mode::Overwrite)?;
            Ok(())
        }
        "all" => {
            generate_tables()?;
            generate_grammar(args);
            generate_parser_tests(Mode::Overwrite)?;
            generate_formatter();
            generate_analyzer()?;
            #[cfg(feature = "configuration")]
            generate_rules_configuration(Mode::Overwrite)?;
            #[cfg(feature = "schema")]
            generate_configuration_schema(Mode::Overwrite)?;
            Ok(())
        }
        _ => {
            eprintln!(
                "\
cargo codegen
Run codegen command.
USAGE:
	cargo codegen <SUBCOMMAND> [option]
SUBCOMMANDS:
	analyzer        Generate factory functions for the analyzer and the configuration of the analyzers
	configuration   Generate the part of the configuration that depends on some metadata
	schema          Generate the JSON schema for the Rome configuration file format
	grammar         Transforms ungram files into AST
	formatter       Generates formatters for each language
	test            Extracts parser inline comments into test files
	unicode         Generates unicode table inside lexer
    all             Run all generators
			"
            );
            Ok(())
        }
    }
}

fn generate_grammar(args: Arguments) {
    let arg_list = args.finish();
    let language_list = arg_list
        .into_iter()
        .filter_map(|arg| arg.to_str().map(|item| item.to_string()))
        .collect::<Vec<_>>();
    let _ = generate_ast(Mode::Overwrite, language_list);
}
