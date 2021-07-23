import React, { useState } from 'react';
import { useDebounce } from 'use-debounce';
import Editor from "@monaco-editor/react";
import './App.css';

function App() {
  const [volatileSource, setSource] = useState(defaultProgram);
  const [source] = useDebounce(volatileSource, 1000);

  return (
    <div className="App">
      <div className="editorWrapper">
        <h4>Slang</h4>
        <Editor
          className="editor"
          defaultValue={source}
          onChange={(value, event) => value === undefined ? setSource("") : setSource(value)}
          options={{theme: "vs-dark"}}
        />
      </div>
      <div className="editorWrapper">
        <h4>Interpreter 2</h4>
        <Editor
          className="editor"
          defaultLanguage="javascript"
          defaultValue="// some comment"
          value={
            //@ts-ignore
            clean(slang.interp2(source))
          }
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
          value={
            //@ts-ignore
            clean(slang.interp3(source))
          }
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
          value={
            //@ts-ignore
            slang.jargon(source)
          }
          options={{readOnly: true,
                    theme: "vs-dark"}}
        />
      </div>
    </div>
  );
}

const clean = (s : string) =>
  s.split("\n")
    .map(s => s.replace(/^\t/, ''))
    .map(s => s.replace(/^\s/, ''))
    .map(s => s.replace(/^\[/, ''))
    .map(s => s.replace(/\]$/, ''))
    .map(s => s.replace(/;/, ''))
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
