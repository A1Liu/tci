import React, { useEffect, useState } from "react";
import { useFileUpload } from "./fileUploadContext";

export default function Terminal() {
  const { addListener } = useFileUpload();
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
      <textarea
        value={content}
        onChange={() => {}}
        readOnly
        className="h-screen w-full"
        style={{
          backgroundColor: "#1E1E1E",
          color: "#00FF00",
          boxSizing: "border-box",
          fontFamily: '"Dank Mono", "Fira Code", monospace',
        }}
      />
    </div>
  );
}
