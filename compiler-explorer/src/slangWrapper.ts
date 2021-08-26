import { jargonStepsT } from "./InterpreterJargon";

//@ts-ignore
export const interp = (s: string) => slang.interp0(s);

//@ts-ignore
export const i2compile = (s: string) => clean(slang.interp2Code(s));

//@ts-ignore
export const i3compile = (s: string) => clean(slang.interp3Code(s));

export const jargonCompile = (s: string) =>
  //@ts-ignore
  removeEmptyLines(slang.jargonCode(s));

export const computeI2steps = (s: string): [string[], string[], string[]][] => {
  //@ts-ignore
  return JSON.parse(slang.interp2(s));
};

export const computeI3steps = (
  s: string
): [string, [number, string[], string[]][]] => {
  //@ts-ignore
  return JSON.parse(slang.interp3(s));
};

export const computeJargonSteps = (s: string): jargonStepsT => {
  //@ts-ignore
  return JSON.parse(slang.jargon(s));
};

const clean = (s: string) =>
  s
    .split("\n")
    .map((s) => s.replace(/^\t/, ""))
    // .map(s => s.replace(/^\s/, ''))
    .map((s) => s.replace(/^\[/, ""))
    .map((s) => s.replace(/\]$/, ""))
    .map((s) => s.replace(/;/, ""))
    .filter((s) => s !== "")
    .join("\n");

const removeEmptyLines = (s: string) =>
  s
    .split("\n")
    .filter((s) => s !== "")
    .join("\n");
