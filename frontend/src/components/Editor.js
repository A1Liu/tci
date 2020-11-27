import AceEditor from "react-ace";
import React from "react";
import "../App.css";
import "ace-builds/src-noconflict/mode-csharp";
import "ace-builds/src-noconflict/theme-monokai";
import { useFileUpload } from "./fileUploadContext";

const parseLineNumber = (code, start, _end) => {
  const lines = code.split("\n");
  let lineNumber = 0;
  let currentChars = 0;
  for (let i = 0; i < lines.length; i += 1) {
    currentChars += lines[i].length + 1;
    lineNumber += 1;
    if (currentChars >= start) {
      return lineNumber;
    }
  }
  return lineNumber;
};

export default function BasicEditor() {
  const {
    files,
    currentFile,
    location,
    addFile,
    sockSend,
    setFiles,
    setCurrentFile,
  } = useFileUpload();
  const code = files[currentFile];
  const rowNumber = parseLineNumber(code.content, location.start, location.end);
  // eslint-disable-next-line no-unused-vars

  const onValueChange = (content) => {
    addFile(currentFile, content);
  };

  const removeFile = (fileId) => {
    sockSend("RemoveFile", fileId);
  };

  const annotations = [
    {
      row: rowNumber, // must be 0 based
      column: 0, // must be 0 based
      text: "current point", // text to show in tooltip
      type: "info",
    },
  ];

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
        mode="csharp"
        theme="monokai"
        onChange={onValueChange}
        value={code.content}
        fontSize={12}
        setOptions={{
          enableLiveAutocompletion: true,
          enableSnippets: true,
          showLineNumbers: true,
          tabSize: 2,
        }}
        style={{ height: "100vh", width: "full" }}
        annotations={annotations}
      />
    </div>
  );
}
