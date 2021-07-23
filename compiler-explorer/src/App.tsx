import React, { useState, useEffect } from 'react';
import { useDebounce } from 'use-debounce';
import Editor from "@monaco-editor/react";
import './App.css';

function App() {
  const [volatileSource, setSource] = useState(defaultProgram);
  const [source] = useDebounce(volatileSource, 1000);

  const [result, setResult] = useState(interp(source));
  const [i2code, setI2code] = useState(i2(source));
  const [i3code, setI3code] = useState(i3(source));
  const [jargonCode, setJargonCode] = useState(jargon(source));

  // Only run this if 'source' changes.
  useEffect(() => {
    setResult(interp(source));
    setI2code(i2(source));
    setI3code(i3(source));
    setJargonCode(jargon(source));
  }, [source])

  return (
    <div className="App">
      <div className="editorWrapper">
        <h4>Slang</h4>
        <Editor
          height="82vh"
          defaultValue={source}
          onChange={(value, event) => value === undefined ? setSource("") : setSource(value)}
          options={{theme: "vs-dark"}}
        />
        <div className="resultBox">
          <p>Result</p>
          <p>{result}</p>
        </div>
      </div>
      <div className="editorWrapper">
        <h4>Interpreter 2</h4>
        <Editor
          className="editor"
          defaultLanguage="javascript"
          defaultValue="// some comment"
          value={i2code}
          options={{readOnly: true,
                    theme: "vs-dark"}}
        />
      </div>
      <div className="editorWrapper">
        <h4>Interpreter 3</h4>
         <Editor
          className="editor"
          defaultLanguage="javascript"
          defaultValue="// some comment"
          value={i3code}
          options={{readOnly: true,
                    theme: "vs-dark"}}
        />
      </div>
      <div className="editorWrapper">
        <h4>Jargon</h4>
        <Editor
          defaultLanguage="javascript"
          className="editor"
          defaultValue="// some comment"
          value={jargonCode}
          options={{readOnly: true,
                    theme: "vs-dark"}}
        />
      </div>
    </div>
  );
}

//@ts-ignore
const interp = (s : string) => slang.interp0(s);
//@ts-ignore
const i2 = (s : string) => clean(slang.interp2(s));
//@ts-ignore
const i3 = (s : string) => clean(slang.interp3(s));
//@ts-ignore
const jargon = (s : string) => removeEmptyLines(slang.jargon(s));

const clean = (s : string) =>
  s.split("\n")
    .map(s => s.replace(/^\t/, ''))
    .map(s => s.replace(/^\s/, ''))
    .map(s => s.replace(/^\[/, ''))
    .map(s => s.replace(/\]$/, ''))
    .map(s => s.replace(/;/, ''))
    .filter(s => s !== "")
    .join("\n")

const removeEmptyLines = (s : string) =>
  s.split("\n")
    .filter(s => s !== "")
    .join("\n")

const defaultProgram = `let fib( m : int) : int = 
    if m = 0
    then 1 
    else if m = 1 
         then 1 
         else fib (m - 1) + fib (m -2) 
         end 
    end 
in 
    fib(1) 
end`
export default App;
