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
  files: { "main.c": initialFile },
  fileIds: {},
  terminal: "",
};

const appReducer = (state = initialState, action) => {
  const { type, payload } = action;

  console.log(action);

  switch (type) {
    case "Debug":
      console.log(payload);
      return state;

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

  return (next) => (action) => {
    const { files } = store.getState();
    const { type, payload } = action;

    switch (type) {
      case "Compile":
        worker.postMessage({ type: "Source", payload: ["main.c", files["main.c"]] });
        break;

      default:
        return next(action);
    }
  };
};

const store = createStore(appReducer, applyMiddleware(tciMiddleware));

export default store;
