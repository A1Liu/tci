import styled from "styled-components";
import { useDispatch, useSelector } from "react-redux";
import { useRef } from "react";

export default function Terminal() {
  const content = useSelector((state) => state.terminal);
  const dispatch = useDispatch();
  const keyPresses = useRef({ command: false, control: false, alt: false });

  const downListener = (e) => {
    switch (e.key) {
      case "Control": // control
        keyPresses.control = true;
        break;
      case "Alt": // option
        keyPresses.alt = true;
        break;
      case "Meta": // command (apple)
        keyPresses.meta = true;
        break;
      default:
        dispatch({ type: "CharIn", payload: e.key });
    }
  };

  const upListener = (e) => {
    switch (e.key) {
      case "Control": // control
        keyPresses.control = false;
        break;
      case "Alt": // option
        keyPresses.alt = false;
        break;
      case "Meta": // command (apple)
        keyPresses.meta = false;
        break;
    }
  };

  return (
    <div
      style={{
        height: "100%",
        width: "100%",
        display: "flex",
        flexDirection: "column",
      }}
    >
      <TerminalTitle>
        <div>Terminal</div>
      </TerminalTitle>
      <TerminalText
        value={content}
        readOnly
        tabIndex={0}
        onKeyDown={downListener}
        onKeyUp={upListener}
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
  flex-grow: 1;
  width: 100%;
  padding: 0.5rem;
`;
