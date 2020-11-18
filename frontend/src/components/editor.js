import Editor from "react-simple-code-editor";
import { useFileUpload } from "./fileUploadContext";

export default function BasicEditor() {
  const {
    files,
    addFile,
    currentFile,
    sockSend,
    highlight,
    styles,
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
        <div
          className="text-white py-1 px-6"
          style={{ backgroundColor: "#1E1E1E" }}
        >
          <div>{currentFile}</div>
        </div>
        <div className="bg-gray-800 w-full flex flex-end">
          <button
            className="bg-blue-600 hover:bg-blue-800 text-white py-1 px-6 ml-auto"
            type="button"
            onClick={compile}
          >
            Run
          </button>
        </div>
      </div>
      <Editor
        value={code}
        onValueChange={onValueChange}
        highlight={() => highlight(code, "c")}
        style={styles.root}
        className="h-screen p-8"
      />
    </div>
  );
}
