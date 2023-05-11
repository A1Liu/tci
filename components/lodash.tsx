export function debounce(callback: () => void, wait: number) {
  let timeoutId: number | undefined = undefined;
  return () => {
    window.clearTimeout(timeoutId);
    timeoutId = window.setTimeout(() => {
      callback();
    }, wait);
  };
}
