import Link from "next/link";
import cx from "classnames";
import styles from "./tci.module.css";
import Editor from "@monaco-editor/react";
import monaco from "monaco-editor";
import React from "react";
import { useCompilerWorker } from "@/components/hooks";
import { CompileResult } from "@/components/compiler.schema";
import { Ast } from "@/components/Ast";
import { ScrollWindow } from "@/components/ScrollWindow";
import { debounce } from "@/components/lodash";

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
        console.log("Compiled ", res.result);
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

      <div style={{ flexGrow: 1, display: "flex", maxHeight: "100%" }}>
        <div style={{ width: "50%", height: "100%" }}>
          <Editor
            // This prevents weird scrolling behavior during resizing and maybe when re-opening the page;
            // nameBox is 2.5rem each, so this just takes up the rest of the screen
            height="calc(100vh - 5rem)"
            language="c"
            defaultValue={INITIAL_TEXT}
            onMount={(editor, monaco) => {
              editorRef.current = editor;

              editor.setValue(
                localStorage.getItem("tciEditorValue") ?? INITIAL_TEXT
              );

              const writeToStorage = debounce(
                () => localStorage.setItem("tciEditorValue", editor.getValue()),
                300
              );
              editor.getModel()?.onDidChangeContent((evt) => writeToStorage());

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

        <div className={"full col gap"} style={{ width: "50%" }}>
          <div className={"row gap"} style={{ minHeight: "33%", flexGrow: 1 }}>
            <ScrollWindow
              title={"Lexed Tokens"}
              className="full rounded border"
              style={{ width: "40%" }}
            >
              {result?.lexer && (
                <pre className="pad">
                  {JSON.stringify(result.lexer, undefined, 2)}
                </pre>
              )}
            </ScrollWindow>

            <div className="full" style={{ width: "60%" }}>
              {result?.parsed_ast && <Ast ast={result?.parsed_ast} />}
            </div>
          </div>

          {result?.errors && (
            <ScrollWindow className={"full rounded border"} title="Error">
              <pre>{JSON.stringify(result.errors)}</pre>
            </ScrollWindow>
          )}
        </div>
      </div>
    </div>
  );
}

export default App;
