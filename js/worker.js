import { get, getMany, set, update } from "idb-keyval";
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
    case "CreateFile": {
      const { name, fd } = paylaod;

      await update("files", (f) => Object.assign(f ?? {}, { [name]: fd }));
      await update(fd, (f) => []);
      break;
    }

    case "ClearFd": {
      await update(payload, (f) => []);
      break;
    }

    case "WriteFd": {
      const { buf, begin, fd } = payload;

      await update(fd, (f) => {
        if (f === undefined) throw new Error("file doesn't exist");

        if (begin > f.length) throw new Error("out of range");

        for (let i = 0; i < buf.length; i++) f[begin + i] = buf[i];

        return f;
      });

      break;
    }

    case "AppendFd": {
      const { buf, fd } = payload;

      await update(fd, (f) => {
        if (f === undefined) throw new Error("file doesn't exist");

        buf.forEach(f.push);
        position = f.length;

        return f;
      });

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

    await runEcall(ecall);
  }
};

const runEnv = async () => {
  const fileDescriptors = [];
  const fileNames = {};
  const fileDataObj = {};

  const files = (await get("files")) ?? {};

  Object.entries(files).forEach(([name, fd]) => {
    fileNames[fd] = name;
    fileDescriptors.push(fd);
  });

  const fileDataArr = await getMany(fileDescriptors);
  fileDataArr.forEach(
    (data, idx) => (fileDataObj[fileDescriptors[idx]] = data)
  );

  const fileName = (fd) => {
    const name = fileNames[fd];
    delete fileNames[fd];
    return name;
  };

  const fileData = (fd) => {
    const data = fileDataObj[fd];
    delete fileDataObj[fd];
    return data;
  };

  return { send, recv, wait, fileName, fileData, fileDescriptors };
};

ecallHandler();
runEnv().then(run);
