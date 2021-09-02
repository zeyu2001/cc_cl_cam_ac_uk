import { useEffect, useRef } from 'react';

type Handler = (event: KeyboardEvent) => void;

export const useKeypress = (keys: string[], handler: Handler) => {
  const eventListenerRef = useRef<Handler>(() => {});

  useEffect(() => {
    eventListenerRef.current = (event: KeyboardEvent) => {
      if (keys.includes(event.key)) {
        handler?.(event);
      }
    };
  }, [keys, handler]);

  useEffect(() => {
    const eventListener = (event: KeyboardEvent) => {
      eventListenerRef.current(event);
    };
    window.addEventListener('keydown', eventListener);
    return () => {
      window.removeEventListener('keydown', eventListener);
    };
  }, []);
};