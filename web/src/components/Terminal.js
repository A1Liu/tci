import React from "react";
import { useSelector } from "react-redux";
import styled from "styled-components";

export default function Terminal() {
  const content = useSelector((state) => state.terminal);

  return (
    <div>
      <TerminalTitle>
        <div>Terminal</div>
      </TerminalTitle>
      <TerminalText
        value={content}
        readOnly
        style={{
          backgroundColor: "#1E1E1E",
          color: "#00FF00",
          boxSizing: "border-box",
          fontFamily: '"Dank Mono", "Fira Code", monospace',
        }}
      />
    </div>
  );
}

const TerminalTitle = styled.div`
  height: 2.5rem;
  --text-opacity: 1;
  color: #fff;
  color: rgba(255, 255, 255, var(--text-opacity));
  --bg-opacity: 1;
  background-color: #424242;
  background-color: rgba(66, 66, 66, var(--bg-opacity));
  padding-top: 0.25rem;
  padding-bottom: 0.25rem;
  padding-left: 1.5rem;
  padding-right: 1.5rem;
  width: 100%;
`;

const TerminalText = styled.textarea`
  height: 100vh;
  width: 100%;
  padding: 0.5rem;
`;
