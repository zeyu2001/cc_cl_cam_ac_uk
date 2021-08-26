import Editor from "@monaco-editor/react";
import { useState, useRef, useEffect } from "react";
import CytoscapeComponent from "react-cytoscapejs";
import Cytoscape from "cytoscape";
//@ts-ignore
import klay from "cytoscape-klay";
import { useDebouncedCallback } from "use-debounce";

import Progress from "./Progress";
import "./Stacks.css";

Cytoscape.use(klay);

export type jargonStepsT = [
  string[],
  {
    stack: string[];
    heap: string[];
    heap_graph: heap_graph;
    cp: number;
    fp: number;
    sp: number;
    status: string;
  }[]
];

type node_type = "H_INT" | "H_BOOL" | "H_CI" | "H_HI" | "H_HEADER";
type heap_node = {
  id: string;
  label: string;
  tp: [node_type];
  pointer: number;
};

type edge_type = "BLOCK" | "POINTER";
type heap_edge = {
  source: string;
  target: string;
  label: string;
  tp: [edge_type];
};

type heap_graph = [heap_node[], heap_edge[]];

type pointers = {
  heap: {
    show: boolean;
    val: number;
  };
  code: {
    showHeap: boolean;
    heap: number;
  };
};

const InterpreterJargon = ({
  steps,
  onClose,
}: {
  steps: jargonStepsT;
  onClose?: () => void;
}) => {
  const [installedCode, stepsList] = steps;

  const installedCodeS = installedCode.join("\n");

  const [step, setStep] = useState(0);

  const { stack, cp, fp, heap, heap_graph } = stepsList[step];

  const envStack = stack
    .slice()
    .reverse()
    .map((s) => s.slice(6))
    .join("\n");

  const currentInst = cp + 1;
  const currentFrame = stack.length - fp;

  const memory = heap.join("\n");
  const showMemory = stepsList.some(({ heap }) => heap.length > 0);

  const setCodeDecorations = useState({})[1];
  const codeEditorRef = useRef<any>(null);
  const codeMonacoRef = useRef<any>(null);

  const setEnvDecorations = useState({})[1];
  const envEditorRef = useRef<any>(null);
  const envMonacoRef = useRef<any>(null);

  const setHeapDecorations = useState({})[1];
  const heapEditorRef = useRef<any>(null);
  const heapMonacoRef = useRef<any>(null);

  const [pointers, setPointers] = useState<pointers>({
    heap: {
      show: false,
      val: 0,
    },
    code: {
      showHeap: false,
      heap: 0,
    },
  });

  const codeEditorDidMount = (editor: any, monaco: any) => {
    codeEditorRef.current = editor;
    codeMonacoRef.current = monaco;
    setCodeDecorations(
      codeEditorRef.current.deltaDecorations({}, [
        {
          range: new codeMonacoRef.current.Range(
            currentInst,
            1,
            currentInst,
            1
          ),
          options: {
            isWholeLine: true,
            linesDecorationsClassName: "currentLineDec",
          },
        },
      ])
    );
  };

  const envEditorDidMount = (editor: any, monaco: any) => {
    envEditorRef.current = editor;
    envMonacoRef.current = monaco;
    setEnvDecorations(envEditorRef.current.getModel().getAllDecorations());
  };

  const heapEditorDidMount = (editor: any, monaco: any) => {
    heapEditorRef.current = editor;
    heapMonacoRef.current = monaco;
    setHeapDecorations(codeEditorRef.current.deltaDecorations({}, []));
  };

  useEffect(() => {
    if (codeEditorRef.current && codeMonacoRef.current) {
      codeEditorRef.current.revealRange(
        new codeMonacoRef.current.Range(currentInst, 1, currentInst, 1)
      );
      codeEditorRef.current.setPosition({
        column: 0,
        lineNumber: currentInst,
      });
      setCodeDecorations((decorations) =>
        codeEditorRef.current.deltaDecorations(decorations, [
          {
            range: new codeMonacoRef.current.Range(
              currentInst,
              1,
              currentInst,
              1
            ),
            options: {
              isWholeLine: true,
              linesDecorationsClassName: "currentLineDec",
            },
          },
          {
            range: new codeMonacoRef.current.Range(
              pointers.code.heap,
              1,
              pointers.code.heap,
              1
            ),
            options: {
              isWholeLine: true,
              linesDecorationsClassName: pointers.code.showHeap
                ? "heapPointerDec"
                : "",
            },
          },
        ])
      );
    }
    if (envEditorRef.current && envMonacoRef.current) {
      setEnvDecorations((decorations) =>
        envEditorRef.current.deltaDecorations(decorations, [
          {
            range: new envMonacoRef.current.Range(
              currentFrame,
              1,
              currentFrame,
              1
            ),
            options: {
              isWholeLine: true,
              linesDecorationsClassName: "framePointerDec",
            },
          },
        ])
      );
    }
    if (heapEditorRef.current && heapMonacoRef.current) {
      if (pointers.heap.show) {
        heapEditorRef.current.revealRange(
          new heapMonacoRef.current.Range(
            pointers.heap.val,
            1,
            pointers.heap.val,
            1
          )
        );
      }
      setHeapDecorations((decorations) =>
        heapEditorRef.current.deltaDecorations(decorations, [
          {
            range: new heapMonacoRef.current.Range(
              pointers.heap.val,
              1,
              pointers.heap.val,
              1
            ),
            options: {
              isWholeLine: true,
              linesDecorationsClassName: pointers.heap.show
                ? "heapPointerDec"
                : "",
            },
          },
        ])
      );
    }
  }, [
    step,
    currentInst,
    currentFrame,
    setCodeDecorations,
    setEnvDecorations,
    setHeapDecorations,
    pointers,
  ]);

  const [nodes, edges] = heap_graph;

  const elements = [
    ...nodes.map((v) => ({
      data: v,
      style: nodeStyle(v.tp[0]),
    })),
    ...edges.map((v) => ({
      data: v,
    })),
  ];

  const onMouseNodeOver = useDebouncedCallback((e: any) => {
    const data = e.target._private.data;
    const isCI = data.tp[0] === "H_CI";
    const pointer = {
      heap: {
        val: parseInt(data.id) + 1,
        show: true,
      },
      code: {
        showHeap: isCI,
        heap: isCI ? data.pointer + 1 : pointers.code.heap,
      },
    };

    setPointers(pointer);
  }, 50);

  const onMouseNodeOff = useDebouncedCallback((e: any) => {
    setPointers({
      heap: {
        val: pointers.heap.val,
        show: false,
      },
      code: {
        showHeap: false,
        heap: pointers.code.heap,
      },
    });
  }, 50);

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
          value={installedCodeS}
          width="33%"
          language="javascript"
          onMount={codeEditorDidMount}
          options={{
            readOnly: true,
            lineNumbers: (lineNumber: number) => (lineNumber - 1).toString(),
            theme: "vs-dark",
          }}
        />
        <Editor
          value={envStack}
          language="javascript"
          width="33%"
          onMount={envEditorDidMount}
          options={{
            readOnly: true,
            lineNumbers: (lineNumber: number) =>
              (envStack.split("\n").length - lineNumber + 1).toString(),
            minimap: { enabled: false },
          }}
        />
        <div className="jargonMem">
          {showMemory ? (
            <Editor
              value={memory}
              height="50%"
              onMount={heapEditorDidMount}
              language="javascript"
              options={{
                readOnly: true,
                lineNumbers: (lineNumber: number) =>
                  (lineNumber - 1).toString(),
                theme: "vs-dark",
                minimap: { enabled: false },
              }}
            />
          ) : null}

          <CytoscapeComponent
            id="heap"
            elements={elements}
            style={{ height: "50%" }}
            cy={(cy) =>
              cy.on("add", "node", (_evt) => {
                cy.layout({
                  name: "klay",
                  //@ts-ignore
                  klay: {
                    direction: "DOWN",
                    borderSpacing: 80,
                    spacing: 80,
                  },
                }).run();
                cy.fit();
                cy.on("mouseover", "node", onMouseNodeOver);
                cy.on("mouseout", "node", onMouseNodeOff);
              })
            }
          />
        </div>
      </div>
      <Progress values={steps[1]} index={step} setIndex={setStep} />
    </div>
  );
};

function nodeStyle(nodeTp: node_type) {
  let style;
  switch (nodeTp) {
    case "H_INT":
      style = {
        shape: "square",
        "background-color": "white",
      };
      break;
    case "H_BOOL":
      style = {
        shape: "barrel",
        "background-color": "purple",
      };
      break;
    case "H_CI":
      style = {
        shape: "tag",
        "background-color": "red",
      };
      break;
    case "H_HI":
      style = {
        shape: "round-tag",
        "background-color": "white",
      };
      break;
    case "H_HEADER":
      style = {
        "background-color": "black",
      };
  }

  style = {
    ...style,
    "text-margin-y": "-10",
    "text-justification": "left",
    "text-wrap": "wrap",
    color: "green",
  };
  return style;
}

export default InterpreterJargon;
