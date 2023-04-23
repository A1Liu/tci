import styles from "./Terminal.module.css";
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
      <div className={styles.TerminalTitle}>
        <div>Terminal</div>
      </div>
      <div
        className={styles.TerminalText}
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
