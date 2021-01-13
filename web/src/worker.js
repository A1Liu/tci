import lib from "../../Cargo.toml";

const postMessage = self.postMessage;

self.onmessage = (e) => {
  postMessage(e.data);
  // lib.wasm.greet();
};

// postMessage("hello");
// setInterval(() => postMessage('test'), 1000);
