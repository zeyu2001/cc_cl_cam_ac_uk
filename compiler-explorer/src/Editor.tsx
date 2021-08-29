import EditorInternal, { EditorProps } from "@monaco-editor/react";

interface IEditorProps extends EditorProps {
  onKeyDown?: (e: KeyboardEvent) => void;
}

const Editor = (props: IEditorProps) => (
  <EditorInternal
    {...props}
    onMount={(e, m) => {
      if (props.onMount) props.onMount(e, m);
      e.onKeyDown(({ browserEvent }: { browserEvent: KeyboardEvent }) => {
        if (props.onKeyDown) props.onKeyDown(browserEvent);
      });
    }}
  />
);

export default Editor;
