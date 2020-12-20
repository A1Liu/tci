use llvm_ir::Module;
use rand::Rng;
use smol::fs;
use smol::process::{Command, Stdio};
use std::collections::HashMap;
use std::io;
use std::path::Path;

const CHARSET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

pub async fn clang_compile(
    test_dir: &Path,
    files: HashMap<&'_ str, &'_ str>,
) -> Result<(Module, String), io::Error> {
    let test_dir = fs::canonicalize(test_dir).await?;

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

        let mut key = Vec::with_capacity(28);
        for _ in 0..28 {
            key.push(CHARSET[rng.gen_range(0..CHARSET.len())]);
        }
        key.extend(".dir".as_bytes());
        let key = unsafe { String::from_utf8_unchecked(key) };
        let key_dir = test_dir.join(&key);
        if let Ok(()) = fs::create_dir(&key_dir).await {
            break key_dir;
        }
    };
    let out_file = key_dir.with_extension(".dum"); // because this whole process is pretty dumb

    for (&path, &content) in files.iter() {
        let res = smol::fs::write(key_dir.join(path), content.as_bytes()).await;
        if let Err(err) = res {
            fs::remove_dir_all(key_dir).await.unwrap();
            return Err(err);
        }
    }

    let mut cmd = Command::new("clang");

    let wdir_flag = "-working-directory=".to_string() + key_dir.to_str().unwrap();
    let out_file_str = out_file.to_str().unwrap();
    cmd.args(["-g", "-c", "-emit-llvm", &wdir_flag, "-o", out_file_str].iter());
    cmd.args(files.keys().map(|f| *f));
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());
    cmd.stdin(Stdio::null());

    let out = cmd.output().await;
    fs::remove_dir_all(key_dir).await.unwrap();
    let out = out?;

    let messages = unsafe { String::from_utf8_unchecked(out.stderr) };
    if !out.status.success() {
        return Err(io::Error::new(io::ErrorKind::Other, messages));
    }

    let result = Module::from_bc_path(&out_file)
        .map(|module| (module, messages))
        .map_err(|string| io::Error::new(io::ErrorKind::Other, string));
    fs::remove_file(&out_file).await.unwrap();
    return result;
}

#[test]
fn test_clang_compile() {
    let mut files = HashMap::new();
    files.insert(
        "main.c",
        "#include <stdio.h>\n\nint main() {\n  printf(\"Hello, world!\\n\");\n  return 0; }",
    );

    smol::block_on(async move {
        let project_directory = Path::new(file!()).parent().unwrap().parent().unwrap();

        let test_dir = project_directory.join("clang-test");
        fs::create_dir(&test_dir).await.unwrap();
        let comp_result = clang_compile(&test_dir, files).await;
        let cleanup_result = fs::remove_dir_all(&test_dir).await;
        let (module, messages) = comp_result.unwrap();
        cleanup_result.unwrap();

        println!("messages: {}", messages);
    });
}
