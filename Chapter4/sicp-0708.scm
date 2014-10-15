\documentclass[a4paper,12pt]{article}
\usepackage{listings}
\title{プログラミング言語課題}\author{1024-29-3152 竹田創}
\begin{document}
\maketitle

\section{problem}
\lstset{numbers=left,basicstyle=\small}
考え方\\
(and x1 x2 .. xn)でxiがすべて真ならば真,一つでも偽であれば偽を返す。\\
evalの定義にandを追加して、apply-contの定義にもeval-andを追加する。\\
で値valが真かどうかを判断し、真なら評価し、偽ならhaltして偽をかえす。

\begin{verbatim}
(define (eval-and exp env cont)
  (if (null? exp) (apply-cont cont true)
      (eval (car exp) env
	    (make-andc (cdr exp) env cont))))

\end{verbatim}
考えかたを計算機科学コース３回生の松村優也君におしえてもらいました。




実行例\\

\begin{verbatim}

;;; CPS-Eval input:
(and 1 2 3 4)

;;; CPS-Eval value:
#t

;;; CPS-Eval input:
(and 1 2 3 false 4)

;;; CPS-Eval value:
#f

;;; CPS-Eval input:
(and true true true true)

;;; CPS-Eval value:
#t
\end{verbatim}



\section{ソースコード}
\lstinputlisting{cps-eval.scm}

\end{document}
