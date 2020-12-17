import React, { useRef } from "react";
import { useDispatch } from "react-redux";
import styled from "styled-components";

export default function NavBar() {
  const hiddenFileInput = useRef(undefined);
  const dispatch = useDispatch();

  const next = () => dispatch({ type: "DebugNext" });
  const prev = () => dispatch({ type: "DebugPrev" });

  const handleOnClick = (event) => {
    event.preventDefault();
    hiddenFileInput.current.click();
  };

  const compile = () => dispatch({ type: "Compile" });

  const readFile = (file) => {
    return new Promise((resolve, reject) => {
      const reader = new FileReader();
      reader.onload = () => resolve(reader.result);
      reader.onerror = (error) => reject(error);
      reader.readAsText(file);
    });
  };

  const handleOnChange = async (event) => {
    Object.values(event.target.files).forEach(async (file) => {
      const data = await readFile(file);
      dispatch({ type: "AddFile", payload: { path: file.name, data } });
    });
  };

  return (
    <nav className="h-12 w-full bg-gray-800 flex p-2 border-b border-gray-600">
      <div className="pb-5 ml-2">
        <input
          style={{ display: "none" }}
          type="file"
          multiple
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

      <StepButton left onClick={prev}>
        <Vertical left />
        <Arrow left />
      </StepButton>

      <PlayButton onClick={compile} />

      <StepButton type="button" onClick={next}>
        <Arrow />
        <Vertical />
      </StepButton>
    </nav>
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
