import Editor from "react-simple-code-editor";
import Highlight, { defaultProps } from "prism-react-renderer";
import theme from "prism-react-renderer/themes/vsDark";
import { useFileUpload } from "./fileUploadContext";

const HighlightedCode = ({ code }) => (
  <Highlight {...defaultProps} theme={theme} code={code} language="c">
    {({ tokens, getLineProps, getTokenProps }) => (
      <>
        {tokens.map((line, i) => (
          <div {...getLineProps({ line, key: i })}>
            {line.map((token, key) => (
              <span {...getTokenProps({ token, key })} />
            ))}
          </div>
        ))}
      </>
    )}
  </Highlight>
);

export default function BasicEditor() {
  const { files, addFile, currentFile, sockSend } = useFileUpload();
  const code = files[currentFile];

  const onValueChange = (content) => {
    addFile(currentFile, content);
  };

  const compile = () => {
    sockSend("Compile", undefined);
  };

  const styles = {
    root: {
      boxSizing: "border-box",
      fontFamily: '"Dank Mono", "Fira Code", monospace',
      ...theme.plain,
      outline: 0,
      overflow: "scroll",
    },
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
        highlight={() => <HighlightedCode code={code} />}
        style={styles.root}
        className="h-screen p-8"
        padding={8}
      />
    </div>
  );
}
