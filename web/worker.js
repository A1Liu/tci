import { run } from "../Cargo.toml";

const postMessage = self.postMessage;
const messages = [];
self.onmessage = (e) => {
  messages.push(e.data);
};

const send = (data) => {
  postMessage(JSON.parse(data));
};
const recv = () => {
  return JSON.stringify(messages.shift());
};
const wait = () => new Promise((resolve) => setTimeout(resolve, 1));

run(send, recv, wait);
