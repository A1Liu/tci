import React from "react";

export function useCompilerWorker(): Worker | undefined {
  const workerRef = React.useRef<Worker>();

  React.useEffect(() => {
    workerRef.current = new Worker(
      new URL("./compiler.worker.ts", import.meta.url)
    );
    workerRef.current.onmessage = (event: MessageEvent<number>) =>
      alert(`WebWorker Response => ${event.data}`);

    return () => {
      workerRef.current?.terminate();
    };
  }, []);

  return workerRef.current;
}
