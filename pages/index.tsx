import Link from "next/link";
import styles from "./tci.module.css";

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
        <Link className={styles.name} href="/">
          TCI
        </Link>
        <Link className={styles.name} href="https://github.com/A1Liu/tci">
          Check out TCI on Github!
        </Link>
      </div>
    </div>
  );
}

export default App;
