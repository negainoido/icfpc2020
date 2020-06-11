use std::fs::File;
use std::path::Path;

use assert_cmd::Command;

use tempfile::tempdir;

#[test]
fn basic() {
    let dir = tempdir().unwrap();

    let input = dir.path().join("input.txt");
    let output = dir.path().join("output.txt");

    File::create(&input).unwrap();

    let mut cmd = Command::cargo_bin("solver1").unwrap();
    cmd.args(&[
        "--input",
        input.to_str().unwrap(),
        "--output",
        output.to_str().unwrap(),
    ]);
    cmd.assert().success();

    assert!(Path::new(&output).exists());

    let mut cmd = Command::cargo_bin("solver1").unwrap();
    cmd.args(&[
        "--input",
        input.to_str().unwrap(),
        "--output",
        output.to_str().unwrap(),
    ]);
    cmd.arg("--verbose").assert().success();

    let mut cmd = Command::cargo_bin("solver1").unwrap();
    cmd.args(&[
        "--input",
        input.to_str().unwrap(),
        "--output",
        output.to_str().unwrap(),
    ]);
    cmd.arg("--num").assert().success();
}
