/* eslint-disable no-console */
/* eslint-disable react/prop-types */
import React, { createContext, useState, useEffect } from "react";

const FileUploadContext = createContext({
  files: [], // array of files
  addFile: (files) => console.log(files),
  compile: (code) => console.log(code),
  addListener: (listener) => console.log(listener),
  response: "",
  socket: {},
});

export const FileUploadProvider = ({ children }) => {
  const [fileList, setFiles] = useState([]);
  const [socket, setSocket] = useState(undefined);
  const [responseStr, setResponse] = useState("");
  const [listeners, setListeners] = useState([]);

  useEffect(() => {
    const sock = new WebSocket("wss://tci.a1liu.com");
    sock.onmessage = (evt) => {
      const resp = JSON.parse(evt.data);
      if (resp.response === "Stdout") {
        console.log(`${resp.data} \n\n`);
      } else if (resp.response === "StatusRet") {
        console.log(`...Program exit with exit code ${resp.data.ret}`);
      } else if (resp.response === "RuntimeError") {
        console.log(resp);
      } else if (
        resp.response !== "FileId" &&
        resp.response !== "Confirm" &&
        typeof sock !== "undefined"
      ) {
        sock.send(
          JSON.stringify({
            command: "RunOp",
          })
        );
      }
      setResponse(resp);
    };
    setSocket(sock);
  }, []);

  const addListener = (listener) => {
    setListeners([...listeners, listener]);
  };

  const addFile = async (fileContents) => {
    const convertFileToString = (uploadedFile) =>
      new Promise((resolve, reject) => {
        const reader = new FileReader();
        if (uploadedFile) {
          reader.readAsText(uploadedFile);
        }
        reader.onload = () => {
          resolve(reader.result);
        };
        reader.onerror = (error) => reject(error);
      });

    const result = await convertFileToString(fileContents);

    setFiles(() => [
      ...fileList,
      {
        name: fileContents.name,
        file: fileContents,
        contents: result,
      },
    ]);
  };

  return (
    <FileUploadContext.Provider
      value={{
        files: fileList,
        addFile,
        response: responseStr,
        socket,
        addListener,
      }}
    >
      {children}
    </FileUploadContext.Provider>
  );
};

export const useFileUpload = () => React.useContext(FileUploadContext);
