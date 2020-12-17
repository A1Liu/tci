import AceEditor from "react-ace";
import { Range } from "ace-builds";
import "../App.css";
import "ace-builds/src-noconflict/mode-csharp";
import "ace-builds/src-noconflict/theme-monokai";
import { useRef, useState } from "react";
import { useDispatch, useSelector } from "react-redux";

const EditorTab = ({ dispatch, file, currentFile, setCurrentFile }) => {
  if (currentFile === file) {
    return (
      <div className="flex flex-row bg-gray-400 h-10 border-r border-l border-gray-500">
        <button type="button" className="py-2 px-2" onClick={setCurrentFile}>
          {file}
        </button>
        <button
          type="button"
          className="bg-transparent text-2xl font-semibold leading-none px-2 outline-none focus:outline-none"
          onClick={() => {
            dispatch({ type: "RemoveFile", payload: file });
          }}
        >
          <span>×</span>
        </button>
      </div>
    );
  }

  return (
    <div className="flex flex-row bg-gray-700 h-10 border-r border-l border-gray-500">
      <button type="button" className="py-2 px-2" onClick={setCurrentFile}>
        {file}
      </button>
      <button
        type="button"
        className="bg-transparent text-2xl font-semibold leading-none px-2 outline-none focus:outline-none"
        onClick={() => {
          dispatch({ type: "RemoveFile", payload: file });
        }}
      >
        <span>×</span>
      </button>
    </div>
  );
};

const BasicEditor = () => {
  const dispatch = useDispatch();
  const files = useSelector((state) => state.files);
  const debugging = useSelector((state) => state.debugging);
  const currentCodeLoc = useSelector((state) => state.currentCodeLoc);
  const fileNames = useSelector((state) => state.fileNames);

  const codeLocRef = useRef(undefined);
  const codeLocChanged = codeLocRef.current !== currentCodeLoc;
  codeLocRef.current = currentCodeLoc;

  const editor = useRef(undefined);
  const marker = useRef(undefined);
  const currentFile = useRef(undefined);
  const [rerender, setRerender] = useState(0);

  const onValueChange = (content) => {
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
      return;
    }

    if (editor.current === undefined) return;

    const ace = editor.current.editor;
    if (!debugging) {
      if (marker.current !== undefined) {
        ace.session.removeMarker(marker.current);
        marker.current = undefined;
      }

      return;
    }

    if (!codeLocChanged) return;
    if (currentCodeLoc === undefined) {
      if (marker.current !== undefined) {
        ace.session.removeMarker(marker.current);
        marker.current = undefined;
      }

      return;
    }

    const nextFile = fileNames[currentCodeLoc.file];
    if (nextFile !== currentFile.current) {
      currentFile.current = undefined;
      ace.setValue(files[nextFile], -1);
      currentFile.current = nextFile;
    }

    const doc = ace.session.getDocument();
    const { row, column } = doc.indexToPosition(currentCodeLoc.start, 0);
    if (marker.current !== undefined) ace.session.removeMarker(marker.current);
    marker.current = ace.session.addMarker(
      new Range(row, 0, row, column),
      "current_line",
      "fullLine"
    );
  };

  setupEditor();

  const [code, readOnly] =
    currentFile.current === undefined
      ? ["", true]
      : [files[currentFile.current], false];

  return (
    <div style={{ height: "100%" }}>
      <div className="h-10 w-full flex">
        <div className="bg-gray-800 w-full text-white">
          <nav className="flex flex-row w-full overflow-auto">
            {Object.keys(files).map((name) => {
              const changeTab = () => {
                if (name !== currentFile.current) {
                  const { session } = editor.current.editor;
                  if (marker.current !== undefined) {
                    session.removeMarker(marker.current);
                    marker.current = undefined;
                  }

                  currentFile.current = name;
                  setRerender(rerender + 1);
                }
              };

              return (
                <EditorTab
                  key={name}
                  dispatch={dispatch}
                  file={name}
                  currentFile={currentFile.current}
                  setCurrentFile={changeTab}
                />
              );
            })}
          </nav>
        </div>
      </div>

      <div className="neutral" />
      <AceEditor
        ref={editor}
        mode="csharp"
        theme="monokai"
        onChange={onValueChange}
        readOnly={readOnly}
        value={code}
        fontSize={12}
        highlightActiveLine={false}
        setOptions={{ showLineNumbers: true, tabSize: 2 }}
        style={{ height: "100%", width: "full" }}
      />
    </div>
  );
};

export default BasicEditor;
