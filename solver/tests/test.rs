use glob::glob;
use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use assert_cmd::Command;

use regex::Regex;
use tempfile::tempdir;

#[test]
fn test_solver() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let d: PathBuf = [env!("CARGO_MANIFEST_DIR"), "tests", "resources"]
        .iter()
        .collect();
    let re = Regex::new("txt$")?;
    for entry in glob(d.join("test*.txt").to_str().unwrap()).unwrap() {
        let mut cmd = Command::cargo_bin("solver")?;
        let input = entry.unwrap();
        cmd.args(&["--input", input.to_str().unwrap()]);
        cmd.assert().success();
        let input_file = input.file_name().unwrap().to_str().unwrap();
        let ans_file = re.replace(input_file, "ans");
        let mut expected = String::new();
        let mut ans = File::open(d.join(ans_file.as_ref()))?;
        ans.read_to_string(&mut expected)?;
        let result = String::from_utf8(cmd.output()?.stdout)?;
        assert_eq!(expected.trim(), result.trim());
    }
    Ok(())
}
