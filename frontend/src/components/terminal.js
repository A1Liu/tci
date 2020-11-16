import React, { Component } from "react";

let distanceFromNewLine = 6;

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
  const character = `${String.fromCharCode(e.keyCode)}`.toLowerCase();
  const result = terminalText.childNodes[0];
  if (e.keyCode === 13) {
    result.nodeValue += "\nroot$ ";
    distanceFromNewLine = 6;
  } else if (e.keyCode === 8) {
    // dont delete if no characters inputted
    if (distanceFromNewLine > 6) {
      result.nodeValue = result.nodeValue.substring(
        0,
        result.nodeValue.length - 1
      );
      distanceFromNewLine -= 1;
    }
  } else {
    result.nodeValue += character;
    distanceFromNewLine += 1;
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
        <div
          id="terminal-title"
          className="h-10 text-white bg-gray-800 py-1 px-6 w-full"
        >
          <div>Terminal</div>
        </div>
        <p id="terminal-text">
          root$ &nbsp;
          <span className="term-caret">&#x2588;</span>
        </p>
      </div>
    );
  }
}

export default Terminal;
