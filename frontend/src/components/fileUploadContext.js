/* eslint-disable no-console */
/* eslint-disable react/prop-types */
import React, { createContext, useEffect, useState, useRef } from "react";

const FileUploadContext = createContext({
  files: {}, // array of files
  currentFile: "",
  replay: "",
  location: {},
  setCurrentFile: (file) => console.log(file),
  setFiles: (files) => console.log(files),
  addFile: (file) => console.log(file),
  addListener: (messages, listener) => console.log(messages, listener),
  sockSend: (command, data) => console.log(command, data),
  addGlobalListener: (listener) => console.log(listener),
  startReplay: (bool) => console.log(bool),
  updateListener: (message) => console.log(message),
  setLocation: (location) => console.log(location),
});

const starter = `// Online C compiler to run C program online
#include <stdio.h>
int main() {
    // Write C code here
    printf("Hello world");
    return 0;
}
`;

export const FileUploadProvider = ({ children }) => {
  const [files, setFiles] = useState({
    "main.c": {
      content: starter,
      fileId: -1,
    },
  });
  const [currentFile, setCurrentFile] = useState("main.c");
  const [replay, setReplay] = useState(false);
  const [location, setLocation] = useState({
    start: -1,
    end: -1,
    file: -1,
  });
  const open = useRef(false);
  const backlog = useRef([]);
  const globalListeners = useRef([]);
  const listeners = useRef({});
  const socket = useRef(undefined);

  const addListener = (m, listener) => {
    const messages = Array.isArray(m) ? m : [m];
    messages.forEach((message) => {
      if (listeners.current[message] === undefined)
        listeners.current[message] = [listener];
      else listeners.current[message].push(listener);
    });
  };

  const updateListener = (message) => {
    if (listeners.current[message] !== undefined) {
      listeners.current[message] = [];
    }
  };

  const addGlobalListener = (listener) => {
    globalListeners.current.push(listener);
  };

  const sockSend = (command, data) => {
    const value = JSON.stringify({ command, data });
    if (!open.current) return backlog.current.push(value);

    if (backlog.current.length !== 0) {
      backlog.current.forEach((item) => socket.current.send(item));
      backlog.current = [];
    }

    return socket.current.send(value);
  };

  if (socket.current === undefined) {
    const createWebSocket = () => {
      const sock = new WebSocket("wss://tci.a1liu.com");
      // const sock = new WebSocket("ws://localhost:4000");

      sock.onopen = (_evt) => {
        console.log("now open for business");

        backlog.current.forEach((item) => sock.send(item));
        backlog.current = [];
        open.current = true;
      };

      sock.onmessage = (evt) => {
        const resp = JSON.parse(evt.data);
        console.log(resp);
        globalListeners.current.forEach((gl) =>
          gl(sockSend, resp.response, resp.data)
        );

        const messageListeners = listeners.current[resp.response];
        if (messageListeners !== undefined)
          messageListeners.forEach((l) => {
            l(sockSend, resp.response, resp.data, replay);
          });
      };

      sock.onclose = () => {
        socket.current = null;
        setTimeout(createWebSocket, 5000);
      };

      socket.current = sock;
    };
    createWebSocket();
  }

  useEffect(() => {
    addListener(
      ["Stdout", "RuntimeError", "CompileError"],
      (_send, resp, data) => {
        console.log(`response: ${resp} with data ${JSON.stringify(data)}`);
      }
    );

    addListener("FileId", (send, resp, data) => {
      const fileName = data.path;
      setFiles((f) => {
        const newFiles = {};
        newFiles[fileName] = {
          content: f[fileName].content,
          fileId: data.file_id,
        };
        return { ...f, ...newFiles };
      });
    });

    sockSend("AddFile", { path: "main.c", data: starter });
  }, []);

  const addFile = (path, contents) => {
    setFiles((f) => {
      const newFiles = {};
      newFiles[path] = {
        content: contents,
        fileId: -1,
      };
      return { ...f, ...newFiles };
    });
    setCurrentFile(path);
    sockSend("AddFile", { path, data: contents });
  };

  const startReplay = (bool) => {
    setReplay(bool);
  };

  return (
    <FileUploadContext.Provider
      value={{
        files,
        currentFile,
        replay,
        location,
        startReplay,
        setFiles,
        setCurrentFile,
        addFile,
        sockSend,
        addListener,
        addGlobalListener,
        updateListener,
        setLocation,
      }}
    >
      {children}
    </FileUploadContext.Provider>
  );
};

export const useFileUpload = () => React.useContext(FileUploadContext);
