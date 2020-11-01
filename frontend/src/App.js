import "./App.css";
import { useEffect, useState } from "react";

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
    <div className="App">
      <header className="App-header">
        <p>
          Edit <code>src/App.js</code> and save to reload.
        </p>

        <button
          type="button"
          onClick={() => {
            socket.send(JSON.stringify({ command: "AddFile", data: {} }));
          }}
        >
          Hello
        </button>

        <p> {message} </p>
      </header>
    </div>
  );
}

export default App;
