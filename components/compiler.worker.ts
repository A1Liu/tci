import { compile } from "tci";
import { CompileCommand, CompilerOutput } from "./compiler.schema";

const post = (message: CompilerOutput) => postMessage(message);

onmessage = (event) => {
  try {
    const parseResult = CompileCommand.safeParse(event.data);
    if (!parseResult.success) {
      post({ kind: "error", error: parseResult.error });
      return;
    }

    const { source } = parseResult.data;
    const result = compile(source);

    post({ kind: "result", result });
  } catch (error) {
    console.log(error);
    post({
      kind: "error",
      error: {
        error,
      },
    });
  }
};

post({ kind: "init" });
