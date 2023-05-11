import React from "react";
import styles from "./ScrollWindow.module.css";
import cx from "classnames";

type ScrollWindowProps = {
  className?: string;
  style?: React.CSSProperties;
  innerClassName?: string;
  innerStyle?: React.CSSProperties;
  title?: React.ReactNode;
  children?: React.ReactNode;
};

export const ScrollWindow = ({
  className,
  style,
  innerClassName,
  innerStyle,
  title,
  children,
}: ScrollWindowProps) => {
  /*  
    The position relative/absolute stuff makes it so that the
    inner div doesn't affect layout calculations of the surrounding div.
    I found this very confusing at first, so here's the SO post that I got it from:
    https://stackoverflow.com/questions/27433183/make-scrollable-div-take-up-remaining-height
    */
  return (
    <div className={cx(className, styles.wrapper)} style={style}>
      <div style={{ position: "absolute", top: 0, right: "3rem", zIndex: 3 }}>
        {title}
      </div>

      <div className={cx(innerClassName, styles.inner)} style={innerStyle}>
        {children}
      </div>
    </div>
  );
};
