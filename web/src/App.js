import "./styles.css";

import { render, h } from "preact";
import store from "./reducers";
import SplitPane from "react-split-pane";
import styled from "styled-components";
import BasicEditor from "./components/Editor";
import NavBar from "./components/NavBar";
import Terminal from "./components/Terminal";

const App = () => {
  return (
    <div>
      <NameBox>
        <Name href="/"> TCI </Name>
      </NameBox>

      <NavBar />

      <SplitPane
        split="vertical"
        defaultSize="50%"
        minSize={200}
        maxSize={-200}
        style={{
          width: "100%",
          height: "100%",
        }}
      >
        <div
          style={{
            width: "100%",
            height: "100%",
          }}
        >
          <BasicEditor />
        </div>

        <div style={{ width: "100%", height: "100%" }}>
          <Terminal />
        </div>
      </SplitPane>
    </div>
  );
}


const NameBox = styled.div`
  display: flex;
  height: 2.5rem;
  padding-left: 0.5rem;
  padding-right: 0.5rem;
  padding-top: 0.5rem;
  padding-bottom: 0.5rem;
  --bg-opacity: 1;
  background-color: #424242;
  background-color: rgba(66, 66, 66, var(--bg-opacity));
  border-bottom-width: 1px;
  --border-opacity: 1;
  border-color: #757575;
  border-color: rgba(117, 117, 117, var(--border-opacity));
`;

const Name = styled.a`
  --text-opacity: 1;
  color: #4299e1;
  color: rgba(66, 153, 225, var(--text-opacity));
  &:hover {
    --text-opacity: 1;
    color: #fff;
    color: rgba(255, 255, 255, var(--text-opacity));
  }
`;

export default App;
