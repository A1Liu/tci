import "./tailwind.css";
import { FileUploadProvider } from "./components/fileUploadContext";
import NavBar from "./components/NavBar";
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
        <NavBar />
        <div className="flex min-h-screen">
          <div className="w-screen">
            <div className="flex md:flex-row">
              <div className="w-full md:w-1/2 text-center border-r border-gray-800">
                <BasicEditor />
              </div>
              <div className="h-screen w-full md:w-1/2 text-center border-solid">
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
