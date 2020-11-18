import React, { useEffect, useState } from "react";
import Editor from "react-simple-code-editor";
import { useFileUpload } from "./fileUploadContext";

export default function Terminal() {
  const { addListener, highlight, styles } = useFileUpload();
  const [content, setContent] = useState("");

  useEffect(() => {
    addListener("Stdout", (send, resp, data) => {
      let currentContent = content;
      currentContent += `${data}`;
      setContent(currentContent);
    });

    addListener("Compiled", (send, _resp, _data) => {
      send("RunOp", undefined);
      setContent("");
    });

    addListener("Status", (send, _resp, _data) => {
      send("RunOp", undefined);
    });

    addListener("CompileError", (send, _resp, data) => {
      setContent(data.rendered);
    });
  }, []);

  return (
    <div>
      <div
        id="terminal-title"
        className="h-10 text-white bg-gray-800 py-1 px-6 w-full"
      >
        <div>Terminal</div>
      </div>
      <Editor
        value={content}
        readOnly="readonly"
        onValueChange={() => {}}
        highlight={() => highlight(content, "bash")}
        style={styles.root}
        className="h-screen"
      />
    </div>
  );
}
