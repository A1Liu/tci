import reduxWebsocket, { connect, send } from "@giantmachines/redux-websocket";
import { applyMiddleware, createStore } from "redux";

const initialFile = `// Online C compiler to run C program online
#include <stdio.h>
int main() {
    // Write C code here
    printf("Hello, world!\\n");
    return 0;
}
`;

const initialState = {
  files: {
    "main.c": initialFile,
  },
  fileNames: {},
  fileIds: {},
  currentFile: "main.c",
  terminal: "",
  currentCodeLoc: undefined,
  debugging: false,
  running: false, // if true, the program should continue running after receiving a status
  paused: false, // only true if running is false.
};

const appReducer = (state = initialState, action) => {
  if (action.type === "REDUX_WEBSOCKET::MESSAGE") {
    const resp = JSON.parse(action.payload.message);
    action = { type: resp.response, payload: resp.data };
  }

  console.log(action);
  const { type, payload } = action;

  switch (type) {
    case "Reset":
      return {
        ...state,
        running: false,
        debugging: false,
        paused: false,
        currentCodeLoc: undefined,
      };
    case "EditFile": {
      const files = { ...state.files };
      if (files[payload.path] === undefined)
        throw new Error("wtf why are you editing a file you don't have");
      files[payload.path] = payload.data;
      return {
        ...state,
        files,
        running: false,
        debugging: false,
        paused: false,
        currentCodeLoc: undefined,
      };
    }
    case "AddFile": {
      const files = { ...state.files };
      files[payload.path] = payload.data;
      const currentFile =
        state.currentFile === undefined ? payload.path : state.currentFile;
      return { ...state, files, currentFile };
    }
    case "FileId": {
      const fileNames = { ...state.fileNames };
      const fileIds = { ...state.fileIds };
      fileNames[payload.file_id] = payload.path;
      fileIds[payload.path] = payload.file_id;
      return { ...state, fileNames, fileIds };
    }
    case "RemoveFile": {
      const fileIds = { ...state.fileIds };
      const fileNames = { ...state.fileNames };
      const files = { ...state.files };
      let { currentFile } = state;

      const fileId = fileIds[payload];
      delete files[payload];
      delete fileIds[payload];
      delete fileNames[fileId];
      if (payload === currentFile) {
        const keys = Object.keys(files);
        if (keys.length === 0) {
          currentFile = undefined;
        } else {
          currentFile = keys[keys.length - 1];
        }
      }

      return { ...state, fileIds, fileNames, files, currentFile };
    }
    case "SetCurrentFile":
      return { ...state, currentFile: action.payload };
    case "ClearFileNames":
      return { ...state, fileNames: {}, fileIds: {} };
    case "Stdout":
      return { ...state, terminal: state.terminal + payload };
    case "Stderr":
      return { ...state, terminal: state.terminal + payload };
    case "Unwind": {
      const newLength = state.terminal.length - payload;
      const terminal = state.terminal.substring(0, newLength);
      return { ...state, terminal };
    }
    case "Compiled":
      return { ...state, terminal: "", running: true };
    case "Compile":
      return {
        ...state,
        running: false,
        debugging: false,
        paused: false,
        currentCodeLoc: undefined,
      };
    case "CompileError":
      return { ...state, terminal: payload.rendered };
    case "RuntimeError":
      return { ...state, running: false, debugging: true };
    case "Status":
      return { ...state, currentCodeLoc: payload.loc };
    case "StatusRet":
      return { ...state, running: false };
    default:
      return state;
  }
};

const reduxWebsocketMiddleware = reduxWebsocket({ reconnectOnClose: true });

const tciMiddleware = (store) => (next) => (action) => {
  if (action.type === "REDUX_WEBSOCKET::MESSAGE") {
    const resp = JSON.parse(action.payload.message);
    action = { type: resp.response, payload: resp.data };
  }

  const { files, running, debugging } = store.getState();
  const { type, payload } = action;

  switch (type) {
    case "REDUX_WEBSOCKET::OPEN":
      Object.entries(files).forEach(([path, fileData]) => {
        const data = { path, data: fileData };
        store.dispatch(send({ command: "AddFile", data }));
      });

      return next({ type: "Reset" });
    case "AddFile":
      store.dispatch(send({ command: type, data: payload }));
      return next(action);
    case "Compiled":
      store.dispatch(send({ command: "RunCount", data: 50 }));
      return next(action);
    case "Status":
      if (running) store.dispatch(send({ command: "RunCount", data: 50 }));
      return next(action);
    case "DebugNext":
      if (!debugging) return undefined;
      return store.dispatch(send({ command: "ForwardsLine", data: 1 }));
    case "DebugPrev":
      if (!debugging) return undefined;
      return store.dispatch(send({ command: "BackLine", data: 1 }));
    case "Compile": {
      Object.entries(files).forEach(([path, data]) => {
        const commandData = { path, data };
        const wsAction = send({ command: "AddFile", data: commandData });
        store.dispatch(wsAction);
      });

      const wsAction = send({ command: "Compile", data: undefined });
      store.dispatch(wsAction);
      return next(action);
    }
    case "RemoveFile": {
      const data = store.getState().fileIds[payload];
      const wsAction = send({ command: type, data });
      store.dispatch(wsAction);
      return next(action);
    }
    default:
      return next(action);
  }
};

const store = createStore(
  appReducer,
  applyMiddleware(reduxWebsocketMiddleware, tciMiddleware)
);

store.dispatch(connect("wss://tci.a1liu.com"));
// store.dispatch(connect("ws://localhost:4000"));

export default store;
