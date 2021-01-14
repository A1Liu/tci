import { h } from "preact";
import { useRef } from "preact/hooks";
import { useDispatch } from "react-redux";
import styled from "styled-components";

export default function NavBar() {
  const hiddenFileInput = useRef(undefined);
  const dispatch = useDispatch();

  const next = () => dispatch({ type: "DebugNext" });
  const prev = () => dispatch({ type: "DebugPrev" });

  const uploadFileTrigger = (event) => {
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

  const uploadFile = async (event) => {
    Object.values(event.target.files).forEach(async (file) => {
      const data = await readFile(file);
      dispatch({ type: "SetFile", payload: { path: file.name, data } });
    });
  };

  return (
    <NavBarDiv>
      <div>
        <input
          style={{ display: "none" }}
          type="file"
          multiple
          ref={hiddenFileInput}
          onChange={uploadFile}
        />

        <UploadButton type="button" onClick={uploadFileTrigger}>
          Upload a File
        </UploadButton>
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
    </NavBarDiv>
  );
}

const NavBarDiv = styled.nav`
  height: 3rem;
  width: 100%;
  display: flex;
  padding: 0.5rem;
  --bg-opacity: 1;
  background-color: #424242;
  background-color: rgba(66, 66, 66, var(--bg-opacity));
  border-bottom-width: 1px;
  --border-opacity: 1;
  border-color: #757575;
  border-color: rgba(117, 117, 117, var(--border-opacity));
`;

const UploadButton = styled.button`
  --text-opacity: 1;
  color: #fff;
  color: rgba(255, 255, 255, var(--text-opacity));
  padding-top: 0.25rem;
  padding-bottom: 0.25rem;
  padding-left: 0.5rem;
  padding-right: 0.5rem;
  border-radius: 0.25rem;

  --bg-opacity: 1;
  background-color: #3182ce;
  background-color: rgba(49, 130, 206, var(--bg-opacity));
  &:hover {
    --bg-opacity: 1;
    background-color: #2c5282;
    background-color: rgba(44, 82, 130, var(--bg-opacity));
  }
`;

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
