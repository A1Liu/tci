import { get, getMany, set, setMany, update } from "idb-keyval";
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

const appReducer = (state = initialState, action) => {
  const { type, payload } = action;

  console.log(action);

  if (!state.initialized) {
    // don't allow edits while state is still uninit
    if (type === "Init") {
      return {
        ...state,
        files: payload,
        current: Object.keys(payload)[0],
        initialized: true,
      };
    }

    return state;
  }

  switch (type) {
    case "Debug":
      console.log(payload);
      return state;

    case "AddFile": {
      const { name, contents } = payload;
      const current = state.current ?? name;
      const files = { ...state.files };
      files[name] = contents;

      return { ...state, current, files };
    }

    case "SetCurrent":
      return { ...state, current: payload };

    case "WriteCurrent":
      const files = { ...state.files };
      files[stat.current] = payload;
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
    const files = await get("source");

    if (files === undefined) {
      await setMany([
        ["sources", { "main.c": undefined }],
        ["source:main.c", initialFile]
      ]);
      const payload = { "main.c": initialFile };
      return store.dispatch({ type: "Init", payload });
    }

    const keys = Object.keys(files).map((file) => "source:" + file);
    const values = await getMany(keys);
    const payload = keys.reduce((acc, k, i) => ((acc[k] = values[i]), acc), {});
    return store.dispatch({ type: "Init", payload });
  };

  setupState();

  return (next) => (action) => {
    const { files, initialized } = store.getState();
    if (!initialized) return next(action);

    const { type, payload } = action;

    switch (type) {
      case "AddFile":
        update("sources", (files) => ({
          ...files,
          [payload.name]: payload.content,
        }));
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
