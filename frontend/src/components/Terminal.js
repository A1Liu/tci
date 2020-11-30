import React, { useEffect, useState } from "react";
import { useFileUpload } from "./fileUploadContext";

export default function Terminal() {
  const { addListener } = useFileUpload();
  const [content, setContent] = useState("");

  useEffect(() => {
    addListener("Stdout", (send, resp, data) => {
      setContent((c) => c + data);
    });

    addListener("Compiled", (send, _resp, _data) => {
      send("RunCount", 50);
      setContent("");
    });

    addListener("Status", (send, _resp, _data) => {
      send("RunCount", 50);
    });

    addListener("CompileError", (send, _resp, data) => {
      setContent(data.rendered);
    });

    addListener("RuntimeError", (send, _resp, data) => {
      setContent(data.rendered);
    });

    addListener("Unwind", (_send, _resp, data) => {
      setContent((c) => c.substring(0, c.length - data));
    });
  }, []);

  return (
    <div>
      <div className="h-10 text-white bg-gray-800 py-1 px-6 w-full">
        <div>Terminal</div>
      </div>
      <textarea
        value={content}
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
