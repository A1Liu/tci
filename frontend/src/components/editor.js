import React, { useEffect, useState } from "react";

export default function Editor() {
  const [socket, setSocket] = useState(undefined);
  useEffect(() => {
    const sock = new WebSocket("wss://tci.a1liu.com");

    sock.onmessage = (evt) => {
      console.log(evt.data);
    };
    console.log(socket);
    setSocket(sock);
  }, []);

  const compile = () => {
    socket.send(
      JSON.stringify({
        command: "Compile",
        data: "",
      })
    );
  };

  return (
    <div>
      <div className="h-10 w-full flex flex border-solid border border-gray-600">
        <div className="bg-gray-700 text-white py-1 px-6">main.c</div>
        <button
          className="bg-blue-700 hover:bg-blue-800 text-white py-1 px-6 ml-auto"
          type="button"
          onClick={compile}
        >
          Run
        </button>
      </div>
    </div>
  );
}
