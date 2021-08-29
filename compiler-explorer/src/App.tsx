import { useState, useEffect } from "react";
import { useDebounce } from "use-debounce";
import Editor, { useMonaco } from "@monaco-editor/react";

import languageDef from "./LanguageDef";
import samplePrograms from "./SamplePrograms";
import { useKeypress } from "./util";
import Interpreter2 from "./Interpreter2";
import Interpreter3 from "./Interpreter3";
import InterpreterJargon from "./InterpreterJargon";
import "./App.css";
import {
  i2compile,
  computeI2steps,
  i3compile,
  computeI3steps,
  interp,
  jargonCompile,
  computeJargonSteps,
} from "./slangWrapper";

const { fib } = samplePrograms;

// TODO: fix slang crash bug
// //@ts-ignore
// for (const property in slang) {
//   //@ts-ignore
//   slang[property] = (...args: any[]) => {
//     try {
//       //@ts-ignore
//       return slang[property](...args)
//     } catch {
//       return "\"stack overflow\""
//     }
//   }
// }

function App() {
  const [volatileSource, setSource] = useState(fib);
  const [source] = useDebounce(volatileSource, 1000);

  const [result, setResult] = useState(interp(source));

  const [i2code, setI2code] = useState(i2compile(source));
  const [i3code, setI3code] = useState(i3compile(source));
  const [jargonCode, setJargonCode] = useState(jargonCompile(source));

  const [i2Steps, setI2Steps] = useState(computeI2steps(source));
  const [i3Steps, setI3Steps] = useState(computeI3steps(source));
  const [jargonSteps, setJargonSteps] = useState(computeJargonSteps(source));

  const [showI2, setShowI2] = useState(false);
  const [showI3, setShowI3] = useState(false);
  const [showJargon, setShowJargon] = useState(false);

  useKeypress(["Escape"], () => {
    setShowI2(false);
    setShowI3(false);
    setShowJargon(false);
  });

  const monaco = useMonaco();

  useEffect(() => {
    monaco?.editor.setTheme("vs-dark");
    monaco?.languages.register({ id: "Slang" });
    monaco?.languages.setMonarchTokensProvider("Slang", languageDef);
  }, [monaco]);

  useEffect(() => {
    setResult(interp(source));
    setI2code(i2compile(source));
    setI3code(i3compile(source));
    setI2Steps(computeI2steps(source));
    setI3Steps(computeI3steps(source));
    setJargonSteps(computeJargonSteps(source));
    setJargonCode(jargonCompile(source));
  }, [source]);

  return (
    <div className="App">
      <div className="editorWrapper">
        <h4>Slang</h4>
        <Editor
          height="86vh"
          defaultValue={source}
          defaultLanguage="Slang"
          onChange={(value, _) =>
            value === undefined ? setSource("") : setSource(value)
          }
          options={{
            theme: "vs-dark",
            minimap: { enabled: false },
          }}
        />
        <div className="resultBox">
          <p>Result: {result}</p>
        </div>
      </div>
      <div className="editorWrapper">
        <h4>Interpreter 2</h4>
        <Editor
          defaultLanguage="javascript"
          height="86vh"
          value={i2code}
          options={{
            readOnly: true,
            theme: "vs-dark",
            minimap: { enabled: false },
          }}
        />
        <button onClick={() => setShowI2(!showI2)}>
          Visualize Interpretation
        </button>
      </div>
      <div className="editorWrapper">
        <h4>Interpreter 3</h4>
        <Editor
          defaultLanguage="javascript"
          value={i3code}
          height="86vh"
          options={{
            readOnly: true,
            theme: "vs-dark",
            minimap: { enabled: false },
          }}
        />
        <button onClick={() => setShowI3(!showI3)}>
          Visualize Interpretation
        </button>
      </div>
      <div className="editorWrapper">
        <h4>Jargon</h4>
        <Editor
          defaultLanguage="javascript"
          value={jargonCode}
          height="86vh"
          options={{
            readOnly: true,
            theme: "vs-dark",
            minimap: { enabled: false },
          }}
        />
        <button onClick={() => setShowJargon(!showJargon)}>
          Visualize Interpretation
        </button>
      </div>
      {showI2 ? (
        <Interpreter2 steps={i2Steps} onClose={() => setShowI2(false)} />
      ) : null}
      {showI3 ? (
        <Interpreter3 steps={i3Steps} onClose={() => setShowI3(false)} />
      ) : null}
      {showJargon ? (
        <InterpreterJargon
          steps={jargonSteps}
          onClose={() => setShowJargon(false)}
        />
      ) : null}
    </div>
  );
}

export default App;
