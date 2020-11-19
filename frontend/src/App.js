import "./tailwind.css";
import { FileUploadProvider } from "./components/fileUploadContext";
import FileUpload from "./components/FileUpload";
import Terminal from "./components/Terminal";
import BasicEditor from "./components/Editor";

function App() {
  return (
    <FileUploadProvider>
      <div>
        <ul className="flex h-10 px-2 py-2 bg-gray-800 border-b border-gray-600">
          <li className="mr-6">
            <a className="text-blue-500 hover:text-white font-sans" href="/">
              TCI
            </a>
          </li>
        </ul>
        <div className="flex min-h-screen flex-wrap">
          <div className="w-full md:w-1/6 bg-gray-800 p-4 text-center text-white">
            <FileUpload />
          </div>
          <div className="md:w-5/6 p-0 text-center text-gray-200">
            <div className="flex md:flex-row flex-wrap">
              <div className="w-full md:w-1/2 text-center border-r border-gray-800">
                <BasicEditor />
              </div>
              <div className="w-full md:w-1/2 text-center border-solid">
                <Terminal />
              </div>
            </div>
          </div>
        </div>
      </div>
    </FileUploadProvider>
  );
}

export default App;
