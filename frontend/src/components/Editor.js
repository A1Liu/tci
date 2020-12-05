import AceEditor from "react-ace";
import { Range } from "ace-builds";
import "../App.css";
import React, { useEffect, useRef, useState } from "react";
import "ace-builds/src-noconflict/mode-csharp";
import "ace-builds/src-noconflict/theme-monokai";
import { useFileUpload } from "./fileUploadContext";

export default function BasicEditor() {
  const {
    files,
    currentFile,
    location,
    replay,
    addFile,
    sockSend,
    setFiles,
    setCurrentFile,
  } = useFileUpload();
  const code = files[currentFile];
  const aceEditor = useRef(null);
  const [markerId, setMarkerId] = useState(null);
  // eslint-disable-next-line no-unused-vars

  const onValueChange = (content) => {
    addFile(currentFile, content);
  };

  const removeFile = (fileId) => {
    sockSend("RemoveFile", fileId);
  };

  useEffect(() => {
    if (aceEditor !== null && replay) {
      if (markerId !== null) {
        aceEditor.current.editor.session.removeMarker(markerId);
      }
      const {
        row,
        column,
      } = aceEditor.current.editor.session
        .getDocument()
        .indexToPosition(location.start, 0);
      const marker = aceEditor.current.editor.session.addMarker(
        new Range(row, 0, row, column),
        "current_line",
        "fullLine"
      );
      setMarkerId(marker);
    }
  }, [location]);

  useEffect(() => {
    if (aceEditor !== null) {
      if (markerId !== null) {
        aceEditor.current.editor.session.removeMarker(markerId);
      }
    }
  }, [code]);

  return (
    <div>
      <div className="h-10 w-full flex">
        <div className="bg-gray-800 w-full text-white">
          <nav className="flex flex-row w-full overflow-auto">
            {Object.keys(files).map((name) => {
              return name === "main.c" ? (
                <div key={name} className="flex flex-row bg-gray-700 h-10">
                  <button
                    type="button"
                    className="mt-1 py-1 px-4"
                    onClick={() => setCurrentFile(name)}
                  >
                    {name}
                  </button>
                </div>
              ) : (
                <div
                  key={name}
                  className="flex flex-row bg-gray-700 h-10 border-r border-l border-gray-500"
                >
                  <button
                    type="button"
                    className="py-2 px-2"
                    onClick={() => setCurrentFile(name)}
                  >
                    {name}
                  </button>
                  <button
                    type="button"
                    className="bg-transparent text-2xl font-semibold leading-none px-2 outline-none focus:outline-none"
                    onClick={() => {
                      const newFiles = Object.keys(files)
                        .filter((file) => file !== name)
                        .reduce((obj, key) => {
                          obj[key] = files[key];
                          return obj;
                        }, {});
                      const keys = Object.keys(newFiles);
                      setCurrentFile(keys[keys.length - 1]);
                      setFiles(newFiles);
                      removeFile(files[name].fileId);
                    }}
                  >
                    <span>×</span>
                  </button>
                </div>
              );
            })}
          </nav>
        </div>
      </div>
      <div className="neutral" />
      <AceEditor
        ref={aceEditor}
        mode="csharp"
        theme="monokai"
        onChange={onValueChange}
        value={code.content}
        readOnly={replay}
        fontSize={12}
        highlightActiveLine={false}
        setOptions={{
          showLineNumbers: true,
          tabSize: 2,
        }}
        style={{ height: "100vh", width: "full" }}
      />
    </div>
  );
}
