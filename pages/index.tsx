import Link from "next/link";
import styles from "./tci.module.css";
import Editor, { Monaco } from "@monaco-editor/react";
import type monaco from "monaco-editor";
import React from "react";
import { useCompilerWorker } from "@/components/hooks";
import { CompileResult, CompilerOutput } from "@/components/compiler.schema";

const INITIAL_TEXT = `
int main() {
  return 0;
}
`;

export function App() {
  const [result, setResult] =
    React.useState<Partial<CompileResult & { error?: any }>>();

  const worker = useCompilerWorker((res) => {
    switch (res.kind) {
      case "init":
        console.log("compiler initialized");
        compile();
        break;
      case "message":
        console.log("message:", res.message);
        break;
      case "error":
        setResult({ error: res.error });
        console.error(res.error);
        break;
      case "result":
        setResult(res.result);
        break;
    }
  });

  const editorRef = React.useRef<monaco.editor.IStandaloneCodeEditor>();

  function compile() {
    worker.current?.postMessage({
      source: editorRef.current?.getValue() ?? "",
    });
  }

  return (
    <div
      style={{
        height: "100%",
        width: "100%",
        display: "flex",
        flexDirection: "column",
      }}
    >
      <div className={styles.nameBox}>
        <Link className={styles.name} href="/">
          TCI - C Compiler
        </Link>

        <Link className={styles.name} href="https://github.com/A1Liu/tci">
          Check out TCI on Github!
        </Link>
      </div>

      <div className={styles.nameBox}>
        <button onClick={compile}>Compile</button>
      </div>

      <div
        style={{
          flexGrow: 1,
          display: "flex",
        }}
      >
        <div style={{ width: "50%" }}>
          <Editor
            height="100%"
            language="C"
            defaultValue={INITIAL_TEXT}
            onMount={(editor, monaco) => {
              editorRef.current = editor;
              compile();
            }}
          />
        </div>

        <div
          style={{
            width: "50%",
            display: "flex",
            flexDirection: "column",
            height: "100%",
            gap: "10px",
          }}
        >
          {result?.lexer && (
            <div className={styles.scrollBox}>
              <p className={styles.title}>Lexed Tokens</p>
              <pre className={styles.text}>
                {JSON.stringify(result.lexer, undefined, 2)}
              </pre>
            </div>
          )}

          {result?.parsed_ast && (
            <div className={styles.scrollBox}>
              <p className={styles.title}>Parsed AST</p>
              <pre className={styles.text}>
                {JSON.stringify(
                  result.parsed_ast.map((obj) => ({
                    ...obj,
                    kind: `${obj.kind.kind}${
                      obj.kind.data ? `,${obj.kind.data}` : ""
                    }`,
                  })),
                  undefined,
                  2
                )}
              </pre>
            </div>
          )}
        </div>
      </div>
    </div>
  );
}

export default App;
