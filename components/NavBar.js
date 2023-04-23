import { useRef } from "react";
import styles from "./NavBar.module.css";
import { useDispatch } from "react-redux";

export default function NavBar() {
  const hiddenFileInput = useRef(undefined);
  const dispatch = useDispatch();

  // const next = () => dispatch({ type: "DebugNext" });
  // const prev = () => dispatch({ type: "DebugPrev" });

  const uploadFileTrigger = (event) => {
    event.preventDefault();
    hiddenFileInput.current.click();
  };

  const compile = () => dispatch({ type: "Run" });

  const readFile = (file) => {
    return new Promise((resolve, reject) => {
      const reader = new FileReader();
      reader.onload = () => resolve(reader.result);
      reader.onerror = (error) => reject(error);
      reader.readAsText(file);
    });
  };

  const uploadFile = async (event) => {
    Object.values(event.target.files).forEach(async (file) => {
      const contents = await readFile(file);
      dispatch({ type: "AddFile", payload: { name: file.name, contents } });
    });
  };

  return (
    <div className={styles.NavBarDiv}>
      <div>
        <input
          style={{ display: "none" }}
          type="file"
          multiple
          ref={hiddenFileInput}
          onChange={uploadFile}
        />

        <button
          className={styles.UploadButton}
          type="button"
          onClick={uploadFileTrigger}
        >
          Upload a File
        </button>
      </div>

      <button className={styles.PlayButton} onClick={compile} />
    </div>
  );
}
