import { get, getMany, set, setMany, update, del } from "idb-keyval";
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
  files: {},
  current: undefined,
  initialized: false,

  terminal: "",
};

const throttleWriteFile = (limit) => {
  const waiting = {};

  return (file, newValue) => {
    const prev = waiting[file];
    waiting[file] = newValue;
    if (prev !== undefined) return;

    setTimeout(() => {
      const f = waiting[file];
      update("source:" + file, (_) => f);
      delete waiting[file];
    }, limit);
  };
};

const writeFile = throttleWriteFile(300);

const appReducer = (state = initialState, action) => {
  const { type, payload } = action;

  // console.log(action);

  if (!state.initialized) {
    if (type === "Init") {
      console.log(action);
      return {
        ...state,
        files: payload,
        current: Object.keys(payload)[0],
        initialized: true,
      };
    }

    // don't allow edits while state is still uninit
    return state;
  }

  switch (type) {
    case "Debug":
      console.log(payload);
      return state;

    case "InvalidInput":
      console.log("invalid input", payload);
      return state;

    case "AddFile": {
      const { name, contents } = payload;
      const current = state.current ?? name;
      const files = { ...state.files };
      files[name] = contents;

      return { ...state, current, files };
    }

    case "DelFile": {
      const files = { ...state.files };
      delete files[payload];
      const current =
        files[state.current] !== undefined
          ? state.current
          : Object.keys(files)[0];

      return { ...state, current, files };
    }

    case "SetCurrent": {
      const current =
        state.files[payload] !== undefined ? payload : state.current;

      return { ...state, current };
    }

    case "WriteCurrent":
      const files = { ...state.files };
      files[state.current] = payload;
      return { ...state, files };

    case "Stdout":
      return { ...state, terminal: state.terminal + payload };
    case "Stderr":
      return { ...state, terminal: state.terminal + payload };

    case "Compiled":
      return { ...state, terminal: "" };
    case "CompileError":
      return { ...state, terminal: payload.rendered };

    default:
      return state;
  }
};

const tciMiddleware = (store) => {
  const worker = new Worker("./worker.js");
  worker.onmessage = (e) => store.dispatch(e.data);

  const setupState = async () => {
    const files = await get("sources");

    if (files === undefined) {
      await setMany([
        ["sources", { "main.c": 0 }],
        ["source:main.c", initialFile],
      ]);
      const payload = { "main.c": initialFile };
      return store.dispatch({ type: "Init", payload });
    }

    const filePaths = Object.keys(files);
    const keys = filePaths.map((file) => "source:" + file);
    const values = await getMany(keys);
    const payload = filePaths.reduce(
      (acc, k, i) => ((acc[k] = values[i]), acc),
      {}
    );
    return store.dispatch({ type: "Init", payload });
  };

  setupState();

  return (next) => (action) => {
    const { files, current, initialized } = store.getState();
    if (!initialized) return next(action);

    const { type, payload } = action;

    switch (type) {
      case "WriteCurrent":
        writeFile(current, payload);
        return next(action);

      case "DelFile":
        update("sources", (files) => {
          delete files[payload];
          return files;
        });
        del("source:" + payload.name);
        return next(action);

      case "AddFile":
        update("sources", (files) => ({
          ...files,
          [payload.name]: 0,
        }));

        update("source:" + payload.name, (_) => payload.contents);
        return next(action);
      case "Run":
        worker.postMessage({ type: "Run", payload: files });
        break;

      default:
        return next(action);
    }
  };
};

const store = createStore(appReducer, applyMiddleware(tciMiddleware));

export default store;
