import React, { useRef, useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import styled from "styled-components";
import { ControlledEditor, monaco } from "@monaco-editor/react";

const EditorTab = ({ index, dispatch, file, currentFile, setCurrentFile }) => {
  return (
    <EditorTabDiv
      role="button"
      tabIndex={index + 1}
      onClick={setCurrentFile}
      onKeyDown={setCurrentFile}
      focused={file === currentFile}
    >
      <p>{file}</p>
      <EditorTabClose
        type="button"
        onClick={() => dispatch({ type: "RemoveFile", payload: file })}
      >
        <span>×</span>
      </EditorTabClose>
    </EditorTabDiv>
  );
};

const BasicEditor = () => {
  const dispatch = useDispatch();
  const files = useSelector((state) => state.files);
  const debugging = useSelector((state) => state.debugging);
  const currentCodeLoc = useSelector((state) => state.currentCodeLoc);
  const message = useSelector((state) => state.message);
  const fileNames = useSelector((state) => state.fileNames);

  const codeLocRef = useRef(undefined);
  const codeLocChanged = codeLocRef.current !== currentCodeLoc;
  codeLocRef.current = currentCodeLoc;

  const editor = useRef(undefined);
  const monacoRef = useRef(undefined);
  const marker = useRef(undefined);
  const currentFile = useRef(undefined);
  const [rerender, setRerender] = useState(0);

  const onValueChange = (ev, content) => {
    if (currentFile.current !== undefined)
      dispatch({
        type: "EditFile",
        payload: { path: currentFile.current, data: content },
      });
  };

  const setupEditor = () => {
    if (currentFile.current === undefined) {
      const keys = Object.keys(files);
      if (keys.length === 0) return;

      [currentFile.current] = keys;
      return;
    }

    if (files[currentFile.current] === undefined) {
      const keys = Object.keys(files);
      const f = keys.length === 0 ? undefined : keys[keys.length - 1];
      currentFile.current = f;
    }

    if (editor.current === undefined) return;

    const model = editor.current.getModel();

    if (!debugging) {
      if (marker.current !== undefined) {
        monacoRef.current.editor.setModelMarkers(model, currentFile, []);
        marker.current = undefined;
      }

      return;
    }

    if (!codeLocChanged) return;
    if (currentCodeLoc === undefined) {
      if (marker.current !== undefined) {
        monacoRef.current.editor.setModelMarkers(model, currentFile, []);
        marker.current = undefined;
      }

      return;
    }

    const nextFile = fileNames[currentCodeLoc.file];
    if (nextFile !== currentFile.current) {
      currentFile.current = undefined;
      model.setValue(files[nextFile]);
      currentFile.current = nextFile;
    }

    const { lineNumber: rowStart, column: columnStart } = model.getPositionAt(
      currentCodeLoc.start
    );
    const { lineNumber: rowEnd, column: columnEnd } = model.getPositionAt(
      currentCodeLoc.end
    );

    if (marker.current !== undefined)
      monacoRef.current.editor.setModelMarkers(model, currentFile, []);

    marker.current = 1;

    // Severities: Hint=1, Info=2, Warning=4, Error=8;
    monacoRef.current.editor.setModelMarkers(model, currentFile, [
      {
        startLineNumber: rowStart,
        startColumn: columnStart,
        endLineNumber: rowEnd,
        endColumn: columnEnd,
        message: message?.message ?? "current position",
        code: message?.short_name ?? "",
        severity: message?.message ? 5 : 2,
      },
    ]);
  };

  setupEditor();

  const [code, readOnly] =
    currentFile.current === undefined
      ? ["", true]
      : [files[currentFile.current], false];

  return (
    <div style={{ height: "100%" }}>
      <EditorNav>
        {Object.keys(files).map((name, index) => {
          const changeTab = () => {
            if (name !== currentFile.current) {
              const session = monacoRef.current.editor;
              if (marker.current !== undefined) {
                session.setModelMarkers(editor.getModel(), currentFile, []);
                marker.current = undefined;
              }

              currentFile.current = name;
              setRerender(rerender + 1);
            }
          };

          return (
            <EditorTab
              key={name}
              index={index}
              dispatch={dispatch}
              file={name}
              currentFile={currentFile.current}
              setCurrentFile={changeTab}
            />
          );
        })}
      </EditorNav>

      <ControlledEditor
        height="100%"
        theme="vs-dark"
        language="c"
        value={code}
        onChange={onValueChange}
        options={{
          readOnly,
        }}
        editorDidMount={(_, editorRef) => {
          monaco.init().then((ref) => {
            monacoRef.current = ref;
          });

          editor.current = editorRef;
        }}
      />
    </div>
  );
};

const EditorNav = styled.nav`
  --bg-opacity: 1;
  background-color: #424242;
  background-color: rgba(66, 66, 66, var(--bg-opacity));
  width: 100%;
  --text-opacity: 1;
  color: #fff;
  color: rgba(255, 255, 255, var(--text-opacity));
  display: flex;
  flex-direction: row;
  overflow: auto;
`;

const EditorTabDiv = styled.div`
  display: flex;
  flex-direction: row;
  align-items: center;
  padding: 0 0.5rem;
  height: 2.5rem;
  border-left-width: 1px;
  border-right-width: 1px;
  --border-opacity: 1;
  border-color: #9e9e9e;
  border-color: rgba(158, 158, 158, var(--border-opacity));
  background-color: ${({ focused }) => (focused ? "gray" : "light-gray")};
`;

const EditorTabClose = styled.button`
  background-color: transparent;
  font-size: 1.5rem;
  font-weight: 600;
  line-height: 1;
  padding-left: 0.5rem;
  padding-right: 0.5rem;
  outline: 2px solid transparent;
  outline-offset: 2px;
  &:focus {
    outline: 2px solid transparent;
    outline-offset: 2px;
  }
`;

export default BasicEditor;
