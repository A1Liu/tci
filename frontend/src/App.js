import "./tailwind.css";
import "./App.css";
import FileUpload from "./components/fileUpload";
import Terminal from "./components/terminal";

function App() {
  return (
    <div>
      <ul className="flex h-10 px-2 py-2">
        <li className="mr-6">
          <a className="text-blue-500 hover:text-blue-800 font-sans" href="/">
            TCI
          </a>
        </li>
      </ul>
      <div className="flex h-screen md:flex-row flex-wrap">
        <div className="w-full md:w-1/6 bg-gray-600 p-4 text-center text-white">
          <FileUpload />
        </div>
        {/* Will replace with Split Component later */}
        <div className="w-full md:w-5/6 bg-gray-500 p-0 text-center text-gray-200">
          <div className="flex h-screen md:flex-row flex-wrap">
            <div className="w-full md:w-1/2 bg-gray-800 p-4 text-center border-solid border-2 border-opacity-50 border-gray-600">
              Editor
            </div>
            <div
              id="terminal-div"
              className="w-full md:w-1/2 bg-gray-800 p-4 text-center border-solid border-2 border-opacity-25 border-gray-600"
            >
              <Terminal />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

export default App;
