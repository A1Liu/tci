import React, { useState, useRef } from "react";
import { useFileUpload } from "./fileUploadContext";

export default function FileUpload() {
  const { files, addFile, socket } = useFileUpload();
  const [showAlert, setShowAlert] = useState(false);
  const hiddenFileInput = useRef(null);

  const submitFile = (path, fileContent) => {
    console.log(fileContent);
    socket.send(
      JSON.stringify({
        command: "AddFile",
        data: {
          path,
          data: fileContent,
        },
      })
    );
  };

  const handleOnClick = (event) => {
    event.preventDefault();
    hiddenFileInput.current.click();
  };

  async function handleOnChange(event) {
    const file = event.target.files[0];
    addFile(file);
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
    submitFile(`${file.name}`, result);
  }

  return (
    <div className="pt-2 pb-5">
      <input
        style={{ display: "none" }}
        type="file"
        ref={hiddenFileInput}
        onChange={handleOnChange}
      />
      <button
        className="bg-blue-600 hover:bg-blue-800 text-white font-bold py-2 px-6 mb-6 rounded "
        type="button"
        onClick={handleOnClick}
      >
        Upload a File
      </button>
      {files.length !== 0 && (
        <div className="flex flex-col">
          {Object.entries(files).map(([idx, file]) => {
            return (
              <div key={idx} className="mb-2">
                {file.name}
              </div>
            );
          })}
        </div>
      )}
      <div>
        {showAlert ? (
          <div className="text-white px-6 py-4 border-0 rounded relative mb-4 bg-red-500">
            <span className="inline-block align-middle mr-8" />
            {/* {`${Response.data}`} */}
            <button
              type="button"
              className="absolute bg-transparent text-2xl font-semibold leading-none right-0 top-0 mt-4 mr-6 outline-none focus:outline-none"
              onClick={() => setShowAlert(false)}
            >
              <span>×</span>
            </button>
          </div>
        ) : null}
      </div>
    </div>
  );
}
