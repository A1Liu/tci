import { useEffect, useState } from "react";
import "./tailwind.css";
import FileUpload from "./components/fileUpload";

function App() {
  const [socket, setSocket] = useState(undefined);
  const [message, setMessage] = useState("");
  useEffect(() => {
    const sock = new WebSocket("ws://127.0.0.1:3000");

    sock.onmessage = (evt) => {
      setMessage(evt.data);
    };

    setSocket(sock);
  }, []);

  return (
    <div>
      <ul className="flex h-10 px-2 py-2">
        <li className="mr-6">
          <a className="text-blue-500 hover:text-blue-800" href="/">
            TCI
          </a>
        </li>
      </ul>
      <div className="flex h-screen md:flex-row flex-wrap">
        <div className="w-full md:w-1/5 bg-gray-400 p-4 text-center text-gray-700">
          <FileUpload />
          <button
            className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
            type="button"
            onClick={() => {
              socket.send(JSON.stringify({ command: "AddFile", data: {} }));
            }}
          >
            Hello
          </button>
          <p> {message} </p>
        </div>
        {/* Will replace with Split Component later */}
        <div className="w-full md:w-4/5 bg-gray-500 p-4 text-center text-gray-200">
          <div className="flex h-screen md:flex-row flex-wrap">
            <div className="w-full md:w-1/2 bg-orange-800 p-4 text-center text-brown-700">
              Editor
            </div>
            <div className="w-full md:w-1/2 bg-orange-900 p-4 text-center text-black-200">
              Terminal
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

export default App;
