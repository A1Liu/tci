import AceEditor from "react-ace";
import { Range } from "ace-builds";
import "../App.css";
import "ace-builds/src-noconflict/mode-csharp";
import "ace-builds/src-noconflict/theme-monokai";
import { useRef } from "react";
import { useDispatch, useSelector } from "react-redux";

const EditorTab = ({ dispatch, file }) => {
  return (
    <div className="flex flex-row bg-gray-700 h-10 border-r border-l border-gray-500">
      <button
        type="button"
        className="py-2 px-2"
        onClick={() => dispatch({ type: "SetCurrentFile", payload: file })}
      >
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

export default function BasicEditor() {
  const dispatch = useDispatch();
  const files = useSelector((state) => state.files);
  const currentFile = useSelector((state) => state.currentFile);
  const debugging = useSelector((state) => state.debugging);
  const currentCodeLoc = useSelector((state) => state.currentCodeLoc);
  const fileNames = useSelector((state) => state.fileNames);

  const marker = useRef(undefined);
  const editor = useRef(undefined);
  const currentFileRef = useRef(currentFile);

  const onValueChange = (content) => {
    if (currentFile !== undefined)
      dispatch({
        type: "EditFile",
        payload: { path: currentFile, data: content },
      });
  };

  const setupEditor = () => {
    if (!debugging) {
      currentFileRef.current = currentFile;
      return;
    }

    const { session } = editor.current.editor;
    if (marker.current !== undefined) session.removeMarker(marker.current);

    if (currentCodeLoc === undefined) {
      marker.current = undefined;
      return;
    }

    const nextFile = fileNames[currentCodeLoc.file];

    if (nextFile !== currentFile) {
      dispatch({ type: "SetCurrentFile", payload: nextFile });
      currentFileRef.current = nextFile;
      return;
    }

    const doc = session.getDocument();
    const { row, column } = doc.indexToPosition(currentCodeLoc.start, 0);
    marker.current = session.addMarker(
      new Range(row, 0, row, column),
      "current_line",
      "fullLine"
    );
  };

  setupEditor();

  const [code, readOnly] =
    currentFileRef.current === undefined
      ? ["", true]
      : [files[currentFileRef.current], false];

  return (
    <div>
      <div className="h-10 w-full flex">
        <div className="bg-gray-800 w-full text-white">
          <nav className="flex flex-row w-full overflow-auto">
            {Object.keys(files).map((name) => {
              return <EditorTab key={name} dispatch={dispatch} file={name} />;
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
        style={{ height: "100vh", width: "full" }}
      />
    </div>
  );
}
