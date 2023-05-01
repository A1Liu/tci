import React from "react";
import { CompileCommand, CompilerOutput } from "./compiler.schema";

type CompilerWorker = {
  postMessage: (c: CompileCommand) => void;
};

export function useCompilerWorker(
  handler: (c: CompilerOutput) => void
): React.MutableRefObject<CompilerWorker | undefined> {
  const workerRef = React.useRef<Worker>();

  React.useEffect(() => {
    const worker = new Worker(new URL("./compiler.worker.ts", import.meta.url));

    worker.onmessage = (event: MessageEvent<any>) => {
      const res = CompilerOutput.safeParse(event.data);
      if (res.success) handler(res.data);
      else handler({ kind: "error", error: res.error });
    };

    workerRef.current = worker;

    return () => {
      worker.terminate();
    };
  }, []);

  return workerRef;
}
