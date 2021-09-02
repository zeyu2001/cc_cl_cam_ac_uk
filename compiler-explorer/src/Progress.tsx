import { ProgressIndicator } from "@atlaskit/progress-indicator";
import "./Stacks.css";
import { useKeypress } from "./util";

const Progress = ({
  values,
  index,
  setIndex,
}: {
  values: any[];
  index: number;
  setIndex: (n: number) => void;
}) => {
  const n = values.length;

  const inc = () => setIndex((((index + 1) % n) + n) % n);
  const dec = () => setIndex((((index - 1) % n) + n) % n);

  useKeypress(["ArrowRight", "ArrowLeft"], (e: KeyboardEvent) => {
    switch (e.key) {
      case "ArrowRight":
        inc();
        break;
      case "ArrowLeft":
        dec();
        break;
    }
  });

  return (
    <div className="progress">
      <button onClick={dec}>{"<"}</button>
      <ProgressIndicator
        selectedIndex={index}
        values={values}
        appearance={"inverted"}
        onSelect={({ index }) => (setIndex ? setIndex(index) : null)}
      />
      <button onClick={inc}>{">"}</button>
    </div>
  );
};

export default Progress;
