import React, { useState, useEffect } from "react";

export default function FileUpload() {
  const [files, setFiles] = useState("");

  const [socket, setSocket] = useState(undefined);
  const [message, setMessage] = useState("");
  useEffect(() => {
    const sock = new WebSocket("wss://tci.a1liu.com");

    sock.onmessage = (evt) => {
      setMessage(evt.data);
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
          reader.readAsDataURL(file);
        }
        reader.onload = () => {
          resolve(reader.result);
        };
        reader.onerror = (error) => reject(error);
      });
    const result = await convertFileToString(files);
    // Look into getting relative path
    submitFile(`./${files.name}`, result);
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
        className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
        type="button"
        onClick={convertFile}
      >
        Submit
      </button>
      <p>{message}</p>
    </div>
  );
}
