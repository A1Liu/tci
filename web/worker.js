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

    return ecalls.push(payload);
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

const runEcall = async (ecall) => {
  const { type, payload } = ecall;
  send({ type: "Debug", payload });

  switch (type) {
    case "Error":
      return ecall;
    case "OpenFd": {
      const { name, openMode } = payload;

      switch (openMode) {
        case 0: {
          // read
          const files = (await get("files")) ?? {};
          const fd = files[name];

          if (fd === undefined) return { type: "Error", payload: 1 }; // doesn't exist

          return { type: "Fd", payload: fd };
        }
        case 1: {
          // create
          let fd;
          await update("files", (f) => {
            const files = f ?? {};

            fd = files[name] ?? Object.keys(files).length;
            files[name] = fd;

            return files;
          });

          await update(fd, (contents) => contents ?? new Uint8Array());

          return { type: "Fd", payload: fd };
        }
        case 2: {
          // create clear
          let fd;
          await update("files", (f) => {
            const files = f ?? {};

            fd = files[name] ?? Object.keys(files).length;
            files[name] = fd;

            return files;
          });

          await update(fd, (contents) => new Uint8Array());

          return { type: "Fd", payload: fd };
        }
        default:
          return { type: "Error", payload: 11 }; // invalid open mode
      }
    }

    case "ReadFd": {
      const { len, buf, begin, fd } = payload;

      send({ type: "Debug", payload: typeof buf });

      break;
    }

    case "WriteFd": {
      const { buf, begin, fd } = payload;

      send({ type: "Debug", payload: typeof buf });

      break;
    }

    case "AppendFd": {
      const { buf, fd } = payload;

      send({ type: "Debug", payload: typeof buf });

      break;
    }

    default:
      break;
  }
};

const ecallHandler = async () => {
  while (true) {
    const ecall = ecalls.shift();
    if (ecall === undefined) {
      await new Promise((resolve) => (resolver.ecallListener = resolve));
      continue;
    }

    const payload = await runEcall(ecall);
    if (resolver.wasmListener !== undefined) {
      resolver.wasmListener(undefined);
      resolver.wasmListener = undefined;
    }

    messages.push({ type: "Ecall", payload });

    await new Promise((resolve) => {
      if (ecalls.length === 0) return resolve(undefined);
      resolver.ecallListener = resolve;
    });
  }
};

ecallHandler();
run({ send, recv, wait });
