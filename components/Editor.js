import Editor from "@monaco-editor/react";
import styles from "./Editor.module.css";
import React from "react";
import { useRef, useState, useCallback } from "react";
import { useDispatch, useSelector } from "react-redux";

const EditorTab = ({ index, dispatch, file, currentFile, setCurrentFile }) => {
  return (
    <div
      className={styles.EditorTabDiv}
      role="button"
      tabIndex={index + 1}
      onClick={setCurrentFile}
      onKeyDown={setCurrentFile}
      focused={String(file === currentFile)}
    >
      <p>{file}</p>
      <div
        className={styles.EditorTabClose}
        type="button"
        onClick={() => dispatch({ type: "DelFile", payload: file })}
      >
        <span>×</span>
      </div>
    </div>
  );
};

const BasicEditor = () => {
  const editorRef = useRef(undefined);
  const [value, setValue] = React.useState("");

  const onValueChange = useCallback((content, ev) => {
    setValue(content);
  }, []);

  return (
    <div style={{ height: "100%" }}>
      <div className={styles.EditorNav}>
        <EditorTab
          key={"test.c"}
          index={"test.c"}
          dispatch={() => {}}
          file={"test.c"}
          currentFile={"test.c"}
          setCurrentFile={() => {}}
        />
      </div>

      <Editor
        height="100%"
        theme="vs-dark"
        language="c"
        value={value}
        options={{ readOnly: false }}
        onChange={onValueChange}
        onMount={(editor, _) => {
          editorRef.current = editor;
        }}
      />
    </div>
  );
};

export default BasicEditor;
