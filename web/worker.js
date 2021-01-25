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

          await update(fd, (contents) => contents ?? []);

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

          await update(fd, (contents) => []);

          return { type: "Fd", payload: fd };
        }
        default:
          return { type: "Error", payload: 11 }; // invalid open mode
      }
    }

    case "ReadFd": {
      const { len, buf, begin, fd } = payload;

      const f = await get(fd);
      if (f === undefined) return { type: "Error", payload: 1 }; // doesn't exist
      if (begin > f.length) return { type: "Error", payload: 5 }; // out of range

      const content = [];
      for (let i = 0; i < len && f[begin + i] !== undefined; i++)
        content[i] = f[begin + i];

      return { type: "ReadFd", payload: { buf, content } };
      break;
    }

    case "WriteFd": {
      const { buf, begin, fd } = payload;

      let error = 0;
      await update(fd, (f) => {
        if (f === undefined) return (out.error = 1); // doesn't exist

        if (begin > f.length) return (out.error = 5); // out of range

        for (let i = 0; i < buf.length; i++) f[begin + i] = buf[i];

        return f;
      });

      if (error !== 0) return { type: "Error", payload: error }; // invalid open mode
      return { type: "Zeroed" };
    }

    case "AppendFd": {
      const { buf, fd } = payload;

      let error = 0;
      let position = 0;
      await update(fd, (f) => {
        if (f === undefined) return (out.error = 1); // doesn't exist

        buf.forEach((b) => f.push(b));
        position = f.length;

        return f;
      });

      if (error !== 0) return { type: "Error", payload: error }; // invalid open mode
      return { type: "AppendFd", payload: { position } };
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

    postMessage({ type: "Debug", payload: ["hello", payload] });
    messages.push({ type: "Ecall", payload });

    await new Promise((resolve) => {
      if (ecalls.length === 0) return resolve(undefined);
      resolver.ecallListener = resolve;
    });
  }
};

ecallHandler();
run({ send, recv, wait });
