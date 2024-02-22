use argh::{FromArgValue, FromArgs};

#[derive(FromArgs, Debug, PartialEq)]
/// A functional computer algebra system focused on polynomials, based on the simply typed lambda calculus.
struct TopLevel {
    #[argh(subcommand)]
    nested: SubCommandEnum,
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand)]
enum SubCommandEnum {
    One(LoadCommand),
    Two(DocsCommand),
}

#[derive(FromArgs, PartialEq, Debug)]
/// Load files. Usage: "poly_lang.exe load <file> [<file2> ...] "
#[argh(subcommand, name = "load")]
struct LoadCommand {
    #[argh(positional)]
    file: String,

    #[argh(positional)]
    more_files : Vec<String>
}

#[derive(FromArgs, PartialEq, Debug)]
/// Documentation. Usage: "poly_lang.exe docs <topic>". Run "poly_lang.exe docs topics" for the list of topics 
#[argh(subcommand, name = "docs")]
struct DocsCommand {
    #[argh(positional)]
    topic: TopicEnum,
}

#[derive(PartialEq, Debug)]
enum TopicEnum {
    ListTopics,
    FirstTopic,
    SecondTopic
}

impl FromArgValue for TopicEnum {
    fn from_arg_value(value: &str) -> Result<Self, String>
    {
        match value {
            "topics" => Ok(TopicEnum::ListTopics),
            "FirstTopic" => Ok(TopicEnum::FirstTopic),
            "SecondTopic" => Ok(TopicEnum::SecondTopic),
            _ => Err("Not a valid topic. Run \"poly_lang.exe docs topics\" for the list of topics".to_string())
        }
    }
}

fn main() {
    let up: TopLevel = argh::from_env();
    eprint!("{:?}", up);
}