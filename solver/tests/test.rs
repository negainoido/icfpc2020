use std::fs::File;
use std::io::Write;
use std::path::Path;

use assert_cmd::Command;

use tempfile::tempdir;

#[test]
fn basic() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let dir = tempdir()?;

    let input = dir.path().join("input.txt");
    let output = dir.path().join("output.txt");
    {
        let mut file = File::create(&input)?;
        write!(file, "test")?;
    }

    let mut cmd = Command::cargo_bin("solver1")?;
    cmd.args(&[
        "--input",
        input.to_str().unwrap(),
        "--output",
        output.to_str().unwrap(),
    ]);
    cmd.assert().success();

    assert!(Path::new(&output).exists());

    let mut cmd = Command::cargo_bin("solver1")?;
    cmd.args(&[
        "--input",
        input.to_str().unwrap(),
        "--output",
        output.to_str().unwrap(),
    ]);
    cmd.arg("--verbose").assert().success();

    let mut cmd = Command::cargo_bin("solver1")?;
    cmd.args(&[
        "--input",
        input.to_str().unwrap(),
        "--output",
        output.to_str().unwrap(),
    ]);
    cmd.arg("--num").assert().success();
    Ok(())
}
