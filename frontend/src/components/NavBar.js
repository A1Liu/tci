import React, { useRef } from "react";
import styled from "styled-components";
import { useFileUpload } from "./fileUploadContext";

export default function NavBar() {
  const {
    replay,
    addFile,
    sockSend,
    startReplay,
    addListener,
    updateListener,
    setLocation,
  } = useFileUpload();
  const hiddenFileInput = useRef(null);

  const handleOnClick = (event) => {
    event.preventDefault();
    hiddenFileInput.current.click();
  };

  const handleCheckBox = () => {
    updateListener("Status");
    if (!replay) {
      addListener("Status", (_send, _resp, data) => {
        setLocation({
          start: data.loc.start,
          end: data.loc.end,
          file: data.loc.file,
        });
      });
    } else {
      addListener("Status", (send, _resp, _data) => {
        send("RunCount", 50);
      });
    }
    startReplay(!replay);
  };

  const compile = () => {
    sockSend("Compile", undefined);
  };

  const moveForward = () => {
    sockSend("RunLine", undefined);
  };

  const moveBackward = () => {
    sockSend("BackLine", 1);
  };

  async function handleOnChange(event) {
    const file = event.target.files[0];
    if (
      file.name.endsWith(".c") ||
      file.name.endsWith(".C") ||
      file.name.endsWith(".h")
    ) {
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

      const result = await convertFileToString(file);
      addFile(file.name, result);
    }
  }

  return (
    <div className="w-full bg-gray-800 border-b border-gray-600">
      <nav className="h-12 w-1/2 justify-between flex p-2 border-r border-gray-600">
        {replay ? (
          <div className="flex justify-between mt-1 w-40">
            <StepButton left onClick={moveBackward}>
              <Vertical left />
              <Arrow left />
            </StepButton>
            <PlayButton onClick={compile} />
            <StepButton type="button" onClick={moveForward}>
              <Arrow />
              <Vertical />
            </StepButton>
          </div>
        ) : (
          <PlayButton onClick={compile} />
        )}
        <div className="flex">
          <div className="mr-2 pt-1">
            <button type="button" onClick={handleCheckBox}>
              <input
                type="checkbox"
                id="replay"
                checked={replay}
                onChange={handleCheckBox}
              />
              <span className="ml-1 text-white">Replay</span>
            </button>
          </div>
          <div className="pb-5 ml-2">
            <input
              style={{ display: "none" }}
              type="file"
              ref={hiddenFileInput}
              onChange={handleOnChange}
            />
            <button
              className="bg-blue-600 hover:bg-blue-800 text-white py-1 px-2 rounded"
              type="button"
              onClick={handleOnClick}
            >
              Upload a File
            </button>
          </div>
        </div>
      </nav>
    </div>
  );
}

const PlayButton = styled.button`
  width: 0;
  height: 24px;
  border-style: solid;
  border-color: transparent transparent transparent #d3d3d3;
  box-sizing: border-box;
  border-width: 12px 0px 12px 18px;
  transition: 200ms all ease;
  margin: 0 10px;
  &:hover {
    border-color: transparent transparent transparent #808080;
  }
`;

const Vertical = styled.span`
  width: 0;
  height: 24px;
  border-style: solid;
  border-color: ${({ left }) =>
    left
      ? "transparent #d3d3d3 transparent transparent"
      : "transparent transparent transparent #d3d3d3"};
  box-sizing: border-box;
  border-width: ${({ left }) =>
    left ? "0px 5px 0px 15px" : "0px 15px 0px 5px"};
`;

const StepButton = styled.button`
  display: flex;
  flex-direction: row;
  margin: 0 5px;
  transition: 200ms all ease;
  &:hover ${Vertical} {
    border-color: ${({ left }) =>
      left
        ? "transparent #808080 transparent transparent"
        : "transparent transparent transparent #808080"};
  }
`;

const Arrow = styled.span`
  width: 0;
  height: 24px;
  border-style: solid;
  border-color: transparent transparent transparent #d3d3d3;
  box-sizing: border-box;
  border-width: 12px 0px 12px 18px;
  transform: ${({ left }) => (left ? "rotate(180deg)" : "rotate(0)")};
  ${StepButton}:hover & {
    border-color: transparent transparent transparent #808080;
  }
`;
