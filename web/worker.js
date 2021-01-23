import { get, set } from "idb-keyval";
import { run } from "../Cargo.toml";

const resolver = {};
const postMessage = self.postMessage;
const indexedDb = self.indexedDb;

const messages = [];
self.onmessage = (e) => {
  if (resolver.current !== undefined) {
    resolver.current();
    resolver.current = undefined;
  }
  messages.push(e.data);
};

const send = (data) => {
  postMessage(data);
};
const recv = () => {
  const msg = messages.shift();
  return msg;
};

const wait = async (timeout) => {
  if (timeout === 0 || timeout === undefined) {
    if (messages.length !== 0) return;
    return await new Promise((resolve) => {
      resolver.current = resolve;
    });
  }

  await new Promise((resolve) => setTimeout(resolve, timeout));
};

const nonstop = async () => {
  while (true) {
    try {
      await run({ send, recv, wait });
    } catch (e) {
      send(e);
    }
  }
};

nonstop();
