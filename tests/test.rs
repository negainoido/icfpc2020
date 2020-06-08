use assert_cmd::Command;

#[test]
fn basic() {
    let mut cmd = Command::cargo_bin("solver1").unwrap();
    cmd.assert().success();

    let mut cmd = Command::cargo_bin("solver1").unwrap();
    cmd.arg("--verbose").assert().success();

    let mut cmd = Command::cargo_bin("solver1").unwrap();
    cmd.arg("--num").assert().success();
}
