import React, { useState, useEffect } from "react";

export default function FileUpload() {
  const [files, setFiles] = useState("");
  const [socket, setSocket] = useState(undefined);
  const [message, setMessage] = useState("");
  const [showAlert, setShowAlert] = useState(false);

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

  async function convertFile(event) {
    event.preventDefault();
    const convertFileToString = (file) =>
      new Promise((resolve, reject) => {
        const reader = new FileReader();
        if (file) {
          reader.readAsText(file);
        }
        reader.onload = () => {
          resolve(reader.result);
        };
        reader.onerror = (error) => reject(error);
      });
    const result = await convertFileToString(files);
    submitFile(`${files.name}`, result);
  }

  return (
    <div className="pt-2 pb-5">
      <input
        className="pb-5"
        type="file"
        multiple
        onChange={(event) => {
          setFiles(event.target.files[0]);
        }}
      />
      <button
        className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 mb-4 rounded "
        type="button"
        onClick={convertFile}
      >
        Upload
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
