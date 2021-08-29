import { ProgressIndicator } from "@atlaskit/progress-indicator";
import "./Stacks.css";
import { useKeypress } from "./util";

export function keyHandler(
  index: number,
  setIndex: (i: number) => void,
  size: number
) {
  const inc = () => setIndex((((index + 1) % size) + size) % size);
  const dec = () => setIndex((((index - 1) % size) + size) % size);

  return (key: string) => {
    switch (key) {
      case "ArrowRight":
        inc();
        break;
      case "ArrowLeft":
        dec();
        break;
    }
  };
}

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

  const handler = keyHandler(index, setIndex, n);
  useKeypress(["ArrowRight", "ArrowLeft"], (e: KeyboardEvent) =>
    handler(e.key)
  );

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
