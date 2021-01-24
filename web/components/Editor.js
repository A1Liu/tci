import { ControlledEditor, monaco } from "@monaco-editor/react";
import { h } from "preact";
import { useRef, useState } from "preact/hooks";
import { useDispatch, useSelector } from "react-redux";
import styled from "styled-components";

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
      {/*<EditorTabClose
        type="button"
        onClick={() => dispatch({ type: "RemoveFile", payload: file })}
      >
        <span>×</span>
      </EditorTabClose>*/}
    </EditorTabDiv>
  );
};

const BasicEditor = () => {
  const dispatch = useDispatch();
  const files = useSelector((state) => state.files);
  const current = useSelector((state) => state.current);

  const editorRef = useRef(undefined);
  const monacoRef = useRef(undefined);

  const onValueChange = (ev, content) => {
    store.dispatch({ type: "WriteCurrent", payload: content });
  };

  return (
    <div style={{ height: "100%" }}>
      <EditorNav>
        {Object.keys(files).map((name, index) => {
          const changeTab = () =>
            dispatch({ type: "SetCurrent", payload: name });

          return (
            <EditorTab
              key={name}
              index={index}
              dispatch={dispatch}
              file={name}
              currentFile={current}
              setCurrentFile={changeTab}
            />
          );
        })}
      </EditorNav>

      <ControlledEditor
        height="100%"
        theme="vs-dark"
        language="c"
        value={files[current]}
        onChange={onValueChange}
        editorDidMount={(_, ref) => {
          monaco.init().then((ref) => {
            monacoRef.current = ref;
          });

          editorRef.current = ref;
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
