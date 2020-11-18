import React, { useEffect, useState } from "react";
import { useFileUpload } from "./fileUploadContext";

export default function Terminal() {
  const { response } = useFileUpload();
  const [content, setContent] = useState("");

  useEffect(() => {
    let currentContent = content;
    if (response.response === "StatusRet") {
      currentContent += `...Program exit with exit code ${response.data.ret}`;
    } else if (response.response === "Stdout") {
      currentContent += `${response.data} \n\n`;
    }
    setContent(currentContent);
  }, [response]);

  return (
    <div>
      <div
        id="terminal-title"
        className="h-10 text-white bg-gray-800 py-1 px-6 w-full"
      >
        <div>Terminal</div>
      </div>
      <div
        className="h-screen w-full p-2"
        style={{ backgroundColor: "#1E1E1E" }}
      >
        {content}
      </div>
    </div>
  );
}
