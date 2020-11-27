import React, { useRef } from "react";
import { useFileUpload } from "./fileUploadContext";

export default function NavBar() {
  const {
    replay,
    addFile,
    sockSend,
    startReplay,
    addListener,
    updateListener,
    setLocation,
  } = useFileUpload();
  const hiddenFileInput = useRef(null);

  const handleOnClick = (event) => {
    event.preventDefault();
    hiddenFileInput.current.click();
  };

  const handleCheckBox = () => {
    updateListener("Status");
    if (!replay) {
      addListener("Status", (_send, _resp, data) => {
        setLocation({
          start: data.loc.start,
          end: data.loc.end,
          file: data.loc.file,
        });
      });
    } else {
      addListener("Status", (send, _resp, _data) => {
        send("RunOp", undefined);
      });
    }
    startReplay(!replay);
  };

  const compile = () => {
    sockSend("Compile", undefined);
  };

  const moveForward = () => {
    sockSend("RunOp", undefined);
  };

  const moveBackward = () => {
    sockSend("Back", 1);
  };

  async function handleOnChange(event) {
    const file = event.target.files[0];
    if (
      file.name.endsWith(".c") ||
      file.name.endsWith(".C") ||
      file.name.endsWith(".h")
    ) {
      const convertFileToString = (uploadedFile) =>
        new Promise((resolve, reject) => {
          const reader = new FileReader();
          if (uploadedFile) {
            reader.readAsText(uploadedFile);
          }
          reader.onload = () => {
            resolve(reader.result);
          };
          reader.onerror = (error) => reject(error);
        });

      const result = await convertFileToString(file);
      addFile(file.name, result);
    }
  }

  return (
    <nav className="h-12 w-full bg-gray-800 flex p-2 border-b border-gray-600">
      <div className="pb-5 ml-2">
        <input
          style={{ display: "none" }}
          type="file"
          ref={hiddenFileInput}
          onChange={handleOnChange}
        />
        <button
          className="bg-blue-600 hover:bg-blue-800 text-white py-1 px-2 rounded"
          type="button"
          onClick={handleOnClick}
        >
          Upload a File
        </button>
      </div>
      <div className="ml-5 pt-1">
        <input
          type="checkbox"
          id="replay"
          checked={replay}
          onChange={handleCheckBox}
        />
        <span className="ml-1 text-white">Replay</span>
      </div>
      {replay ? (
        <>
          <button
            className="bg-blue-600 hover:bg-blue-800 text-white text-bold py px-4 rounded ml-8"
            type="button"
            onClick={moveBackward}
          >
            Prev
          </button>
          <button
            className="bg-blue-600 hover:bg-blue-800 text-white text-bold py px-4 rounded ml-8"
            type="button"
            onClick={compile}
          >
            Run
          </button>
          <button
            className="bg-blue-600 hover:bg-blue-800 text-white text-bold py px-4 rounded ml-8"
            type="button"
            onClick={moveForward}
          >
            Next
          </button>{" "}
        </>
      ) : (
        <button
          className="bg-blue-600 hover:bg-blue-800 text-white text-bold py px-4 rounded ml-8"
          type="button"
          onClick={compile}
        >
          Play
        </button>
      )}
    </nav>
  );
}
