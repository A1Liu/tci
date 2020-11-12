import React, { useState, useEffect, useRef } from "react";

export default function FileUpload() {
  const [socket, setSocket] = useState(undefined);
  const [message, setMessage] = useState("");
  const [showAlert, setShowAlert] = useState(false);
  const hiddenFileInput = useRef(null);

  useEffect(() => {
    const sock = new WebSocket("wss://tci.a1liu.com");

    sock.onmessage = (evt) => {
      const resp = JSON.parse(evt.data);
      setMessage(resp);
      if (resp.response) {
        setShowAlert(true);
      }
    };

    setSocket(sock);
  }, []);

  const submitFile = (path, fileContent) => {
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
        className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-6 mb-6 rounded "
        type="button"
        onClick={handleOnClick}
      >
        Upload a File
      </button>

      <div>
        {showAlert ? (
          <div className="text-white px-6 py-4 border-0 rounded relative mb-4 bg-red-500">
            <span className="inline-block align-middle mr-8">
              {`${message.data}`}
            </span>
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
