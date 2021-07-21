import React, { useState } from 'react';
import { useDebounce } from 'use-debounce';
import Editor from "@monaco-editor/react";
import './App.css';

function App() {
  const [volatileSource, setSource] = useState(defaultProgram);
  const [source] = useDebounce(volatileSource, 1000);

  function handleEditorDidMount(editor, monaco) {
    editorRef.current = editor; 
  }

//   if (editorRef.current !== null) {
//     editorRef.current.deltaDecorations([], [
//       { range: new monaco.Range(3,1,5,1), options: { isWholeLine: true, linesDecorationsClassName: 'myLineDecoration' }},
//     ]);
//   }

  return (
    <div className="App">
      <div className="editorWrapper">
        <h>Slang</h>
        <Editor
          className="editor"
          height="100vh"
          defaultLanguage="javascript"
          defaultValue={source}
          onChange={(value, event) => value === undefined ? setSource("") : setSource(value)}
          options={{theme: "vs-dark"}}
        />
      </div>
      <div className="editorWrapper">
        <h>Interpreter 2</h>
        <Editor
          height="100vh"
          className="editor"
          defaultLanguage="javascript"
          defaultValue="// some comment"
          value={clean(slang.interp2(source))}
          options={{readOnly: true}}
        />
      </div>
      <div className="editorWrapper">
        <h>Interpreter 3</h>
         <Editor
          height="100vh"
          className="editor"
          defaultLanguage="javascript"
          defaultValue="// some comment"
          value={clean(slang.interp3(source))}
          options={{readOnly: true}}
        />
      </div>
      <div className="editorWrapper">
        <h>Jargon</h>
        <Editor
          height="100vh"
          defaultLanguage="javascript"
          className="editor"
          defaultValue="// some comment"
          value={clean(slang.jargon(source))}
          options={{readOnly: true}}
        />
      </div>
    </div>
  );
}

const clean = s =>
  s.split("\n")
    // .map(s => s.replace(/^\t/, ''))
    // .map(s => s.replace(/^\s/, ''))
    // .map(s => s.replace(/\[/, ''))
    // .map(s => s.replace(/\]/, ''))
    // .map(s => s.replace(/\;/, ''))
    // .filter(s => s !== "")
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
