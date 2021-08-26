import Editor from "@monaco-editor/react";
import { useState, useRef, useEffect } from "react";

import Progress from "./Progress";

import "./Stacks.css";

type Code = string;
type CodePointer = number;
type EnvStack = string[];
type Memory = string[];
type Steps = [Code, [CodePointer, EnvStack, Memory][]];

const Interpreter3 = ({
  steps,
  onClose,
}: {
  steps: Steps;
  onClose?: () => void;
}) => {
  let [installedCode, stepList] = steps;

  installedCode = clean(installedCode);

  const [step, setStep] = useState(0);

  const [currentInst, envStack, memory] = stepList[step];
  const envStackS = envStack.join("\n");
  const memoryS = memory.join("\n");
  const showMem = stepList.some(([_, __, s]) => s.length > 0);

  const setDecorations = useState({})[1];
  const editorRef = useRef<any>(null);
  const monacoRef = useRef<any>(null);

  const codeEditorDidMount = (editor: any, monaco: any) => {
    editorRef.current = editor;
    monacoRef.current = monaco;
    setDecorations(
      editorRef.current.deltaDecorations({}, [
        {
          range: new monacoRef.current.Range(currentInst, 1, currentInst, 1),
          options: {
            isWholeLine: true,
            linesDecorationsClassName: "currentLineDec",
          },
        },
      ])
    );
  };

  useEffect(() => {
    if (editorRef.current && monacoRef.current) {
      editorRef.current.revealRange(
        new monacoRef.current.Range(currentInst, 1, currentInst, 1)
      );
      editorRef.current.setPosition({
        column: 0,
        lineNumber: currentInst,
      });
      setDecorations((decorations) =>
        editorRef.current.deltaDecorations(decorations, [
          {
            range: new monacoRef.current.Range(
              currentInst + 1,
              1,
              currentInst + 1,
              1
            ),
            options: {
              isWholeLine: true,
              linesDecorationsClassName: "currentLineDec",
            },
          },
        ])
      );
    }
  }, [step, currentInst, setDecorations]);

  return (
    <div className="interpreter">
      <div className="interpreterTitle">
        <h3>
          Step {step} - {}
        </h3>
        {onClose ? <button onClick={onClose}>X</button> : null}
      </div>
      <div className="interpreterEditors">
        <Editor
          value={installedCode}
          language="javascript"
          onMount={codeEditorDidMount}
          options={{
            readOnly: true,
            lineNumbers: (lineNumber: number) => (lineNumber - 1).toString(),
            theme: "vs-dark",
          }}
        />
        <Editor
          value={envStackS}
          language="javascript"
          options={{
            readOnly: true,
            lineNumbers: (lineNumber: number) =>
              envStackS.split("\n").length - lineNumber + 1,
            minimap: { enabled: false },
          }}
        />
        {showMem ? (
          <Editor
            value={memoryS}
            options={{
              readOnly: true,
              lineNumbers: (lineNumber: number) => (lineNumber - 1).toString(),
              theme: "vs-dark",
              minimap: { enabled: false },
            }}
          />
        ) : null}
      </div>
      <Progress values={steps[1]} index={step} setIndex={setStep} />
    </div>
  );
};

function clean(code: string): string {
  return code
    .split("\n")
    .map((i) => i.split(" ").slice(1).join(" "))
    .filter((i) => i.length > 0)
    .join("\n");
}

export default Interpreter3;
