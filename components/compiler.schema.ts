import { z } from "zod";

export type CompileCommand = z.infer<typeof CompileCommand>;
export const CompileCommand = z.object({
  source: z.string(),
});

export type CompileResult = z.infer<typeof CompileResult>;
export const CompileResult = z.object({
  source: z.string(),
  lexer: z.array(z.string()),
  parsed_ast: z.array(
    z.object({
      parent: z.number(),
      kind: z.object({
        kind: z.string(),
        data: z.any().optional(),
      }),
    })
  ),
});

export type CompilerOutput = z.infer<typeof CompilerOutput>;
export const CompilerOutput = z.union([
  z.object({ kind: z.literal("init") }),
  z.object({
    kind: z.literal("result"),
    result: CompileResult,
  }),
  z.object({
    kind: z.literal("error"),
    error: z.any().optional(),
  }),
  z.object({
    kind: z.literal("message"),
    message: z.string(),
  }),
]);
