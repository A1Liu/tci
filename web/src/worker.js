import { run } from "../../Cargo.toml";

const postMessage = self.postMessage;

self.onmessage = async (e) => { postMessage(e.data); };

const enq = (fd) => { postMessage(`enqueue called with ${fd}`); };
const deq = () => {};
const wait = () => new Promise(resolve => setTimeout(resolve, 1));

run(enq, deq, wait);
postMessage("goodbye");
