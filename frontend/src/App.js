import "./tailwind.css";
import FileUpload from "./components/fileUpload";

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
        <div className="w-full md:w-1/5 bg-gray-400 p-4 text-center text-gray-700">
          <FileUpload />
        </div>
        {/* Will replace with Split Component later */}
        <div className="w-full md:w-4/5 bg-gray-500 p-0 text-center text-gray-200">
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
