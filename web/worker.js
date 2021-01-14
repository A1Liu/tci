import { run } from "../Cargo.toml";

const resolver = {};
const postMessage = self.postMessage;
const messages = [];
self.onmessage = (e) => {
  if (resolver.current !== undefined) {
    resolver.current();
    resolver.current = undefined;
  }
  messages.push(e.data);
};

const send = (data) => {
  postMessage(JSON.parse(data));
};
const recv = () => {
  return JSON.stringify(messages.shift());
};
const wait = async (timeout) => {
  if (timeout === 0 || timeout === undefined) {
    if (messages.length !== 0) return;
    return await new Promise((resolve) => { resolver.current = resolve; });
  }

  await new Promise((resolve) => setTimeout(resolve, timeout));
};

run(send, recv, wait);
