import React, { Component } from "react";

function caretToggle() {
  const caret = document.getElementsByClassName("term-caret")[0];
  if (caret.classList.contains("blink")) {
    caret.classList.remove("blink");
  } else {
    caret.classList.add("blink");
  }
}

function logKey(e) {
  const terminalText = document.querySelector("#terminal-text");
  const character = `${String.fromCharCode(e.keyCode)}`;
  const result = terminalText.childNodes[0];
  if (e.keyCode === 13) {
    result.nodeValue += "\nROOT$ ";
  } else if (e.keyCode === 8) {
    // dont delete if no characters inputted
    if (result.nodeValue.length > 7) {
      result.nodeValue = result.nodeValue.substring(
        0,
        result.nodeValue.length - 1
      );
    }
  } else {
    result.nodeValue += character;
    console.log(character);
  }
}

class Terminal extends Component {
  constructor(props) {
    super(props);
    this.state = {};
  }

  componentDidMount() {
    document.addEventListener("keydown", logKey);
    setInterval(caretToggle, 500);
  }

  render() {
    return (
      <div>
        <p id="terminal-title">Terminal</p>
        <p id="terminal-text">
          ROOT$
          <span className="term-caret">&#x2588;</span>
        </p>
      </div>
    );
  }
}
export default Terminal;
