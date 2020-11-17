import React, { useEffect, useState } from "react";
import Editor from "react-simple-code-editor";
import Highlight, { defaultProps } from "prism-react-renderer";
import theme from "prism-react-renderer/themes/vsDark";

import { useFileUpload } from "./fileUploadContext";

const starter = `
// Online C compiler to run C program online
#include <stdio.h>

int main() {
    // Write C code here
    printf("Hello world");

    return 0;
}
`;

export default function BasicEditor() {
  const [socket, setSocket] = useState(undefined);
  const [code, setCode] = useState(starter);
  // eslint-disable-next-line no-unused-vars
  const { files } = useFileUpload();

  const styles = {
    root: {
      boxSizing: "border-box",
      fontFamily: '"Dank Mono", "Fira Code", monospace',
      ...theme.plain,
      outline: 0,
      overflow: "scroll",
    },
  };

  const onValueChange = (content) => {
    setCode(content);
  };

  useEffect(() => {
    const sock = new WebSocket("wss://tci.a1liu.com");

    sock.onmessage = (evt) => {
      const resp = JSON.parse(evt.data);
      console.log(resp);
    };
    setSocket(sock);
  }, []);

  const compile = () => {
    socket.send(
      JSON.stringify({
        command: "AddFile",
        data: {
          path: "main.c",
          data: code,
        },
      })
    );

    socket.send(
      JSON.stringify({
        command: "Compile",
      })
    );
  };

  const highlight = (currCode) => (
    <Highlight {...defaultProps} theme={theme} code={currCode} language="c">
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

  return (
    <div>
      <div className="h-10 w-full flex">
        <div
          className="text-white py-1 px-6"
          style={{ backgroundColor: "#1E1E1E" }}
        >
          <div>main.c</div>
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
        highlight={highlight}
        style={styles.root}
        className="h-screen p-8"
      />
    </div>
  );
}
