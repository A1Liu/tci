import SplitPane from "react-split-pane";
import styles from "./tci.module.css";
import BasicEditor from "@/components/Editor";
import NavBar from "@/components/NavBar";
import Terminal from "@/components/Terminal";

export function App() {
  return (
    <div
      style={{
        height: "100%",
        width: "100%",
        display: "flex",
        flexDirection: "column",
      }}
    >
      <div className={styles.nameBox}>
        <a className={styles.name} href="/">
          TCI
        </a>
        <a className={styles.name} href="https://github.com/A1Liu/tci">
          Check out TCI on Github!
        </a>
      </div>

      <NavBar />

      <div style={{ position: "relative", flexGrow: 1 }}>
        <SplitPane
          split="vertical"
          defaultSize="50%"
          minSize={200}
          maxSize={-200}
          style={{
            width: "100%",
            height: "100%",
          }}
        >
          <div
            style={{
              width: "100%",
              height: "100%",
            }}
          >
            <BasicEditor />
          </div>

          <div style={{ width: "100%", height: "100%" }}>
            <Terminal />
          </div>
        </SplitPane>
      </div>
    </div>
  );
}

export default App;
