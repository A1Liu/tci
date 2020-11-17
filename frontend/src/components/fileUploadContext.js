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
    const convertFileToString = (uploadedFile) =>
      new Promise((resolve, reject) => {
        const reader = new FileReader();
        if (uploadedFile) {
          reader.readAsText(uploadedFile);
        }
        reader.onload = () => {
          resolve(reader.result);
        };
        reader.onerror = (error) => reject(error);
      });

    const result = await convertFileToString(fileContents);

    setFiles(() => [
      ...fileList,
      {
        name: fileContents.name,
        file: fileContents,
        contents: result,
      },
    ]);
  };

  return (
    <FileUploadContext.Provider value={{ files: fileList, addFile }}>
      {children}
    </FileUploadContext.Provider>
  );
};

export const useFileUpload = () => React.useContext(FileUploadContext);
