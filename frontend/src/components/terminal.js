import React, { Component } from "react";

const output = [[]];
let lineNum = 0;

function caretToggle() {
  const caret = document.getElementsByClassName("term-caret")[0];
  if (caret.classList.contains("blink")) {
    caret.classList.remove("blink");
  } else {
    caret.classList.add("blink");
  }
}

function unwind(num) {
  const terminalText = document.querySelector("#terminal-text");
  const result = terminalText.childNodes[0];
  let newNum = num;
  // dont let unwind if nothing to unwind
  if (result.nodeValue.length <= 6) {
    result.nodeValue = "root$ ";
    return;
  }
  // delete entire line if unwind is large enough
  if (num > output[lineNum].length) {
    newNum -= output[lineNum].length;
    result.nodeValue = result.nodeValue.substring(
      0,
      result.nodeValue.length - output[lineNum].length - 7
    );
    // do not remove last entry in output
    if (output.length > 1) {
      output.pop();
      lineNum -= 1;
    } else {
      output[lineNum] = [];
    }
    // if there is more characters to delete continue deletion
    if (newNum >= 0) {
      unwind(newNum);
    }
  } else if (output[lineNum].length > 0) {
    result.nodeValue = result.nodeValue.substring(
      0,
      result.nodeValue.length - newNum
    );
    output[lineNum] = output[lineNum].slice(0, -newNum);
  }
}

function logKey(e) {
  const terminalText = document.querySelector("#terminal-text");
  const character = `${String.fromCharCode(e.keyCode)}`.toLowerCase();
  const result = terminalText.childNodes[0];
  if (e.keyCode === 13) {
    // enter pressed
    let command = output[lineNum].join("");
    command = command.replace(/\s/g, ""); // remove spaces
    if (command === "unwind") {
      unwind(5 + 6);
    } else {
      lineNum += 1;
      output.push([]);
      result.nodeValue += "\nroot$ ";
    }
  } else if (e.keyCode === 8) {
    // dont delete if there is nothing to delete
    if (output[lineNum].length > 0) {
      output[lineNum].pop();
      result.nodeValue = result.nodeValue.substring(
        0,
        result.nodeValue.length - 1
      );
    }
  } else {
    output[lineNum].push(character);
    result.nodeValue += character;
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
