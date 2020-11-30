import "./tailwind.css";
import "./Split.css";
import SplitPane from "react-split-pane";
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
        <div className="flex min-h-screen flex-wrap ">
          <div className="md:w-full">
            <div className="flex md:flex-row flex-wrap">
              <SplitPane
                split="vertical"
                defaultSize="50%"
                minSize={200}
                maxSize={-200}
                style={{ width: "100%", height: "100%" }}
              >
                <div
                  style={{ width: "100%", height: "100%" }}
                  className="w-full md:w-1/2 text-center border-r border-gray-800"
                >
                  <BasicEditor />
                </div>
                <div
                  style={{ width: "100%", height: "100%" }}
                  className="h-screen w-full md:w-1/2 text-center border-solid"
                >
                  <Terminal />
                </div>
              </SplitPane>
            </div>
          </div>
        </div>
      </div>
    </FileUploadProvider>
  );
}

export default App;
