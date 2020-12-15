import { useSelector } from "react-redux";

export default function Terminal() {
  const content = useSelector((state) => state.terminal);

  return (
    <div>
      <div className="h-10 text-white bg-gray-800 py-1 px-6 w-full">
        <div>Terminal</div>
      </div>
      <textarea
        value={content}
        readOnly
        className="h-screen w-full p-2"
        style={{
          backgroundColor: "#1E1E1E",
          color: "#00FF00",
          boxSizing: "border-box",
          fontFamily: '"Dank Mono", "Fira Code", monospace',
        }}
      />
    </div>
  );
}
