/* eslint-disable no-console */
/* eslint-disable react/prop-types */
import React, { createContext, useState } from "react";

const FileUploadContext = createContext({
  files: [], // array of files
  addFile: (files) => console.log(files),
});

export const FileUploadProvider = ({ children }) => {
  const [fileList, setFiles] = useState([]);

  const addFile = async (fileContents) => {
    const read = new FileReader();

    read.onload = (e) => {
      setFiles(() => [
        ...fileList,
        {
          name: fileContents.name,
          file: fileContents,
          contents: e.target.result,
        },
      ]);
    };

    read.readAsText(fileContents);
  };

  return (
    <FileUploadContext.Provider value={{ files: fileList, addFile }}>
      {children}
    </FileUploadContext.Provider>
  );
};

export const useFileUpload = () => React.useContext(FileUploadContext);
