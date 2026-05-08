use std::borrow::Cow;
use std::env;
use std::os::unix::process::CommandExt;
use std::process::Command;

use anyhow::{anyhow, Result};
use regex::Regex;

const EMACSCLIENT: &str = "emacsclient";

fn main() -> Result<()> {
    let args: Vec<_> = env::args().skip(1).collect();
    let mut v = Vec::new();

    let mut create_frame = true;
    let mut quiet = true;

    let mut arg;

    if args.len() == 1 {
        arg = Cow::from(args[0].as_str());

        if arg == "--kill" {
            create_frame = false;
            arg = "(kill-emacs)".into();
        } else if arg == "--daemon" {
            create_frame = false;
            quiet = false;
            v.push("-a");
            v.push("");
            arg = "nil".into();
        } else {
            let rx = Regex::new(r#"["\\]"#).unwrap();
            arg = Cow::from(format!(r#"(find-file "{}")"#, rx.replace_all(&arg, "\\$0")));
        }

        if !arg.is_empty() {
            v.push("-e");
            v.push(&arg);
        }
    } else if args.is_empty() {
        v.push("-e");
        v.push("(buffer-menu 1)");
    }

    if quiet {
        v.splice(1..1, ["-q", "-u"]);
    }

    if create_frame {
        v.insert(1, "-c");
    }

    Err(anyhow!(
        "Failed to execute \"{}\": {}",
        EMACSCLIENT,
        Command::new(EMACSCLIENT).args(v).exec()
    ))
}
