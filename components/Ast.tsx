import { AstNode } from "./compiler.schema";
import React from "react";
import styles from "Ast.module.css";
import { ScrollWindow } from "./ScrollWindow";

const DisplayAstNode = ({
  kind,
  children,
}: {
  kind: string;
  children?: React.ReactNode;
}) => {
  return (
    <div
      style={{
        display: "flex",
        flexDirection: "column",
      }}
    >
      <pre>{kind}</pre>

      <div
        style={{
          display: "flex",
          flexDirection: "column",
          paddingLeft: "10px",
          borderLeft: "3px solid black",
        }}
      >
        {children}
      </div>
    </div>
  );
};

const RecursiveAst = ({ ast }: { ast: AstNode[] }) => {
  let rootStack = [];

  // Assumes that nodes are in post order
  for (const node of ast) {
    const data = node.kind.data ? `,${JSON.stringify(node.kind.data)}` : "";
    const kind = `${node.kind.kind}${data}`;

    let index = rootStack.length;
    while (index > 0 && rootStack[index - 1].parent === node.post_order) {
      index -= 1;
    }

    rootStack.push({
      parent: node.parent,
      node: (
        <DisplayAstNode key={`${node.post_order}`} kind={kind}>
          {rootStack.splice(index).map((node) => node.node)}
        </DisplayAstNode>
      ),
    });
  }

  return <>{rootStack.map((root) => root.node)}</>;
};

const FlatAst = ({ ast }: { ast: AstNode[] }) => {
  return (
    <div
      style={{
        display: "flex",
        flexDirection: "column",
        padding: "20px",
        gap: "5px",
      }}
    >
      {ast.map((obj, index) => {
        const data = obj.kind.data ? `,${JSON.stringify(obj.kind.data)}` : "";
        return (
          <pre
            key={`${index}`}
            style={{ padding: "4px", background: "lightblue" }}
          >
            kind: {`${obj.kind.kind}${data}`}
            {"\n"}
            parent: {obj.parent}
          </pre>
        );
      })}
    </div>
  );
};

export const Ast = ({ ast }: { ast: AstNode[] }) => {
  const [recursive, setRecursive] = React.useState(true);

  return (
    <ScrollWindow
      className="full"
      style={{ borderRadius: "4px", border: "2px solid black", height: "100%" }}
      title={
        <div
          style={{
            display: "flex",
            alignItems: "center",
            gap: "5px",
            right: "2rem",
          }}
        >
          <button
            onClick={() => setRecursive((r) => !r)}
            style={{
              color: "white",
              background: "blue",
              borderRadius: "3px",
              padding: "3px",
              lineHeight: "0.7rem",
              textAlign: "center",
              verticalAlign: "center",
            }}
          >
            {recursive ? "r" : "f"}
          </button>

          <p>Parsed AST</p>
        </div>
      }
    >
      <div style={{ padding: "10px" }}>
        {recursive ? <RecursiveAst ast={ast} /> : <FlatAst ast={ast} />}
      </div>
    </ScrollWindow>
  );
};
