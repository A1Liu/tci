import Link from "next/link";
import styles from "./tci.module.css";
import Editor from "@monaco-editor/react";
import type monaco from "monaco-editor";
import React from "react";
import { useCompilerWorker } from "@/components/hooks";
import { CompileResult } from "@/components/compiler.schema";
import { Ast } from "@/components/Ast";

const INITIAL_TEXT = `// Write C code here
int main(int argc, char** argv) {
  return 0;
}
`;

export function App() {
  const [result, setResult] = React.useState<CompileResult>();

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
        console.log("Error returned");
        console.error(res.error);
        break;
      case "result":
        console.log("Compiled");
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
            language="c"
            defaultValue={INITIAL_TEXT}
            onMount={(editor, monaco) => {
              editorRef.current = editor;
              monaco.editor.addKeybindingRules([
                {
                  keybinding: monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS,
                  // TODO: make this do something useful
                  command: "editor.action.formatDocument",
                },
              ]);
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
          <div
            style={{
              display: "flex",
              gap: "10px",
              minHeight: "33%",
              flexGrow: 1,
            }}
          >
            <div className={styles.scrollBox} style={{ width: "40%" }}>
              <p className={styles.title}>Lexed Tokens</p>
              {result?.lexer && (
                <pre className={styles.text}>
                  {JSON.stringify(result.lexer, undefined, 2)}
                </pre>
              )}
            </div>

            <div style={{ height: "100%", width: "60%" }}>
              {result?.parsed_ast && <Ast ast={result?.parsed_ast} />}
            </div>
          </div>

          {result?.error && (
            <div className={styles.scrollBox}>
              <p className={styles.title}>Error</p>
              <pre className={styles.text}>{JSON.stringify(result.error)}</pre>
            </div>
          )}
        </div>
      </div>
    </div>
  );
}

export default App;
