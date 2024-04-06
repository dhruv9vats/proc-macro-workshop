// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run

fn main() {
    use derive_builder::Builder;

    #[derive(Builder)]
    pub struct Command {
        executable: String,
        #[builder(each = "arg")]
        args: Vec<String>,
        #[builder(each = "env")]
        env: Vec<String>,
        current_dir: Option<String>,
    }

    let command = Command::builder()
        .executable("cargo".to_owned())
        .arg("build".to_owned())
        .arg("--release".to_owned())
        .build()
        .unwrap();

    assert!(command.current_dir.is_none());

    assert_eq!(command.executable, "cargo");

    #[derive(Builder)]
    struct Nope(u32);
}
