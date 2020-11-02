import React, { useState } from "react";

export default function FileUpload() {
  const [files, setFiles] = useState([]);

  const handleOnUpload = (event) => {
    setFiles(event.target.files);
    console.log(files);
  };

  return (
    <div className="pt-2 pb-5">
      <input type="file" multiple onChange={handleOnUpload} />
    </div>
  );
}
