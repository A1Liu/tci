import { useCompilerWorker } from "@/components/hooks";
import "./globals.css";
import type { AppProps } from "next/app";

export default function App({ Component, pageProps }: AppProps) {
  useCompilerWorker();

  return <Component {...pageProps} />;
}
