use rand::Rng;
use smol::fs::{canonicalize, create_dir, remove_dir_all};
use smol::process::{Command, Stdio};
use std::io;
use std::path::Path;

const CHARSET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
const KEY_LEN: usize = 30;

pub struct File<'a, 'b> {
    path: &'a str,
    content: &'b str,
}

pub struct ClangOutput {
    success: bool,
    output: Vec<u8>,
    messages: String,
}

pub async fn clang_compile<'a, 'b>(
    test_dir: &Path,
    files: &[File<'a, 'b>],
) -> Result<ClangOutput, io::Error> {
    let test_dir = canonicalize(test_dir).await?;

    let mut rng = rand::thread_rng();
    let mut counter: u8 = 0;
    let key_dir = loop {
        if counter > 10 {
            // contention is impossibly high or we are hella unlucky
            return Err(io::Error::new(
                io::ErrorKind::TimedOut,
                "timed out after 10 tries",
            ));
        }
        counter += 1;

        let mut key = Vec::with_capacity(30);
        for _ in 0..KEY_LEN {
            key.push(CHARSET[rng.gen_range(0..CHARSET.len())]);
        }
        let key = unsafe { String::from_utf8_unchecked(key) };

        let key_dir = test_dir.join(&key);
        if let Ok(()) = create_dir(&key_dir).await {
            break key_dir;
        }
    };

    for file in files {
        let res = smol::fs::write(key_dir.join(file.path), file.content.as_bytes()).await;
        if let Err(err) = res {
            remove_dir_all(key_dir).await.unwrap();
            return Err(err);
        }
    }

    let mut cmd = Command::new("clang");

    let wdir_flag = "-working-directory=".to_string() + key_dir.to_str().unwrap();
    println!("{:?}", wdir_flag);
    cmd.args(["-c", "-emit-llvm", &wdir_flag, "-o", "/dev/stdout"].iter());
    cmd.args(files.iter().map(|f| f.path));
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());
    cmd.stdin(Stdio::null());

    let out = cmd.output().await;
    remove_dir_all(key_dir).await.unwrap();
    let out = out?;

    return Ok(ClangOutput {
        success: out.status.success(),
        output: out.stdout,
        messages: unsafe { String::from_utf8_unchecked(out.stderr) },
    });
}

#[test]
fn test_clang_compile() {
    let file = File {
        path: "main.c",
        content:
            "#include <stdio.h>\n\nint main() {\n  printf(\"Hello, world!\\n\");\n  return 0; }",
    };
    let files = &[file];

    smol::block_on(async move {
        let project_directory = Path::new(file!()).parent().unwrap().parent().unwrap();

        let test_dir = project_directory.join("clang-test");
        create_dir(&test_dir).await.unwrap();
        let comp_result = clang_compile(&test_dir, files).await;
        let cleanup_result = remove_dir_all(&test_dir).await;
        let result = comp_result.unwrap();
        cleanup_result.unwrap();

        println!("messages: {}", result.messages);
        assert!(result.success);
    });
}
