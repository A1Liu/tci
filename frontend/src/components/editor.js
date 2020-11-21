import AceEditor from "react-ace";

import "ace-builds/src-noconflict/mode-csharp";
import "ace-builds/src-noconflict/theme-monokai";
import { useFileUpload } from "./fileUploadContext";

export default function BasicEditor() {
  const {
    files,
    addFile,
    currentFile,
    sockSend,
    setFiles,
    setCurrentFile,
  } = useFileUpload();
  const code = files[currentFile];
  // eslint-disable-next-line no-unused-vars

  const onValueChange = (content) => {
    addFile(currentFile, content);
  };

  const compile = () => {
    sockSend("Compile", undefined);
  };

  return (
    <div>
      <div className="h-10 w-full flex">
        <div className="bg-gray-800 w-full text-white">
          <nav className="flex flex-row">
            {Object.keys(files).map((name) => {
              return name === "main.c" ? (
                <div className="flex flex-row bg-gray-700 h-10 border-r border-l border-gray-50">
                  <button
                    type="button"
                    className="mt-1 py-1 px-4"
                    onClick={() => setCurrentFile(name)}
                  >
                    {name}
                  </button>
                </div>
              ) : (
                <div className="flex flex-row bg-gray-700 h-10 border-r border-l border-gray-50">
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
                    }}
                  >
                    <span>×</span>
                  </button>
                </div>
              );
            })}
          </nav>
        </div>
        <div className="bg-gray-800 flex flex-end">
          <button
            className="bg-blue-600 hover:bg-blue-800 text-white py-1 px-6 ml-auto"
            type="button"
            onClick={compile}
          >
            Run
          </button>
        </div>
      </div>
      <AceEditor
        mode="csharp"
        theme="monokai"
        onChange={onValueChange}
        value={code}
        fontSize={12}
        setOptions={{
          enableLiveAutocompletion: true,
          enableSnippets: true,
          showLineNumbers: true,
          tabSize: 2,
        }}
        style={{ height: "100vh", width: "full" }}
      />
    </div>
  );
}
