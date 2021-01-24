import { get, set, update } from "idb-keyval";
import { run } from "../Cargo.toml";

const resolver = { wasmListener: undefined, ecallListener: undefined };
const postMessage = self.postMessage;

const ecalls = [];
const messages = [];

self.onmessage = (e) => {
  if (resolver.wasmListener !== undefined) {
    resolver.wasmListener(undefined);
    resolver.wasmListener = undefined;
  }

  messages.push(e.data);
};

const send = (data) => {
  const { type, payload } = data;
  if (type === "Ecall") {
    if (resolver.ecallListener !== undefined) {
      resolver.ecallListener(undefined);
      resolver.ecallListener = undefined;
    }

    ecalls.push(payload);
    return;
  }

  return postMessage(data);
};

const recv = () => messages.shift();

const wait = (timeout) => {
  if (timeout === 0 || timeout === undefined) {
    if (messages.length !== 0) return;

    return new Promise((resolve) => {
      resolver.wasmListener = resolve;
    });
  }

  return new Promise((resolve) => setTimeout(resolve, timeout));
};

const ecallHandler = async () => {
  while (true) {
    const ecall = ecalls.shift();
    if (ecall === undefined) {
      await new Promise((resolve) => { resolver.ecallListener = resolve; });
      continue;
    }

    const { type, payload } = ecall;
    send(ecall);
  }
};

ecallHandler();
run({ send, recv, wait });
