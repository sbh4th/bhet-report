# DAGs for supplement

# Policy on BP
\begin{tikzpicture}[shorten > = 1pt, line width=1pt]
\tikzstyle{every node} = [rectangle, fill=white, draw=none, font=\small\sffamily]
\node (x) at (-0.5,0) [align=center] {Policy};
\node (g) at (-0.5,-2) [align=center] {Time-invariant\\village factors};
\node (st) at (-0.5,2) [align=center] {Secular\\trends};
\node (mp) at (4,0) [align=center] {Blood\\pressure};
\node (no) at (4,2) [align=center] {Sex};
\node (a) at (1.5,1) [align=center] {Age};
\node (mt) at (4,-2) [align=left] {Antihypertensive\\medication use};
\node (y) at  (7,1) [align=center] {Exposure to\\tobacco};
\node (w) at (1.5,-1) [align=center] {Waist\\circumference};
\node (u) at (7, -1) [align=center] {Alcohol\\consumption};
\foreach \from/\to in {x/mp, g/x, mt/mp, u/mp, y/mp, no/mp, y/mp, w/mp, a/mp, st/x}
  \draw [->] (\from) -- (\to);
\draw [->] (st.east) to [bend left=35] (mp.north west);
\draw [->] (g.east) to [bend right=35] (mp.south west);
\end{tikzpicture}

# Policy on respiratory health
\begin{tikzpicture}[shorten > = 1pt, line width=1pt]
\tikzstyle{every node} = [rectangle, fill=white, draw=none, font=\small\sffamily]
\node (x) at (-0.5,0) [align=center] {Policy};
\node (g) at (-0.5,-2) [align=center] {Time-invariant\\village factors};
\node (st) at (-0.5,2) [align=center] {Secular\\trends};
\node (mp) at (4,0) [align=center] {Respiratory\\symptoms};
\node (no) at (4,2) [align=center] {Sex};
\node (a) at (1.5,1) [align=center] {Age};
\node (mt) at (4,-2) [align=left] {Frequency of\\farming};
\node (y) at  (7,1) [align=center] {Exposure to\\tobacco};
\node (w) at (1.5,-1) [align=center] {Occupation};
\node (u) at (7, -1) [align=center] {Alcohol\\consumption};
\foreach \from/\to in {x/mp, g/x, mt/mp, u/mp, y/mp, no/mp, y/mp, w/mp, a/mp, st/x}
  \draw [->] (\from) -- (\to);
\draw [->] (st.east) to [bend left=35] (mp.north west);
\draw [->] (g.east) to [bend right=35] (mp.south west);
\end{tikzpicture}

# Policy on Indoor Temp
\begin{tikzpicture}[shorten > = 1pt, line width=1pt]
\tikzstyle{every node} = [rectangle, fill=white, draw=none, font=\small\sffamily]
\node (x) at (-1,0) [align=center] {Policy};
\node (g) at (-1,-2) [align=center] {Time-invariant\\village factors};
\node (st) at (-1,2) [align=center] {Secular\\trends};
\node (t) at  (2,0) [align=center] {Stove use};
\node (mp) at  (5,0) [align=center] {Indoor\\temperature};
\node (no) at (5,2) [align=center] {Number of\\occupants in \\winter};
\node (mt) at (5,-2) [align=left] {Age of primary\\respondent};
\node (y) at  (8,1) [align=center] {Size of home\\(no. rooms)};
\node (w) at (1.5,-1.5) [align=center] {Outdoor\\temperature};
\node (u) at (8, -1) [align=center] {SES};
\foreach \from/\to in {g/x, w/t, mt/mp, u/mp, y/mp, no/mp, t/mp, x/t, st/x}
  \draw [->] (\from) -- (\to);
\draw [->] (x.north east) to [bend left=35] (mp.north west);
\draw [->] (st.east) to [bend left=35] (mp.north);
\draw [->] (g.east) to [bend right=35] (mp.south west);
\end{tikzpicture}

# Mediation of BP by temp and air pollution
\begin{tikzpicture}[shorten > = 1pt, line width=1pt]
\tikzstyle{every node} = [rectangle, fill=white, draw=none, font=\small\sffamily]
\node (x) at (-1,0) [align=center] {Policy};
\node (g) at (-1,-2) [align=center] {Time-invariant\\village factors};
\node (st) at (-1,2) [align=center] {Secular\\trends};
\node (t) at  (1.5,0) [align=center] {Stove use};
\node (bp) at (7.5,0) [align=center] {Blood\\pressure};
\node (mp) at  (4.5,0.75) [align=center] {Indoor\\temperature};
\node (ap) at  (4.5,-0.75) [align=center] {Indoor air\\pollution};
\node (no) at (4.5,2.5) [align=center] {Age};
\node (wc) at (6.5,2.5) [align=center] {Waist\\circumference};
\node (mt) at (5,-2.75) [align=left] {Tobacco\\exposure};
\node (y) at  (8.5,1.5) [align=center] {Antihypertensive\\medication};
\node (w) at (2,-1.5) [align=center] {Time\\of day};
\node (u) at (8.5, -2) [align=center] {Alcohol\\consumption};
\node (s) at (9.5, 0) [align=center] {Sex};
\foreach \from/\to in {g/x, st/x, w/mp, w/ap, mt/ap, u/bp, y/bp, no/mp, t/mp, t/ap, x/t, no/bp, wc/bp, mt/bp, mp/bp, ap/bp, s/bp, wc/mp}
  \draw [->] (\from) -- (\to);
\draw [->] (x.north east) to [bend left=35] (bp.north);
\draw [->] (st.east) to [bend left=25] (bp.north west);
\draw [->] (g.east) to [bend right=35] (bp.south west);
\draw [->] (w.east) to [bend right=15] (bp.south);
\end{tikzpicture}

# Mediation of CHP on BP by mixed combustion
\begin{tikzpicture}[shorten > = 1pt, line width=1pt]
\tikzstyle{every node} = [rectangle, fill=white, draw=none, font=\small\sffamily]
\node (x) at (-0.5,0) [align=center] {Policy};
\node (g) at (-0.5,-2) [align=center] {Time-invariant\\village factors};
\node (st) at (-0.5,2) [align=center] {Secular\\trends};
\node (ms) at (1.9,0) [align=center] {Mixed source\\component};
\node (mp) at (4.5,0) [align=center] {Blood\\pressure};
\node (no) at (5,2) [align=center] {Sex};
\node (a) at (3,2) [align=center] {Tobacco\\exposure};
\node (mt) at (5,-2) [align=left] {Antihypertensive\\medication use};
\node (y) at  (7,1) [align=center] {Age};
\node (w) at (2.5,-3) [align=center] {Waist\\circumference};
\node (u) at (7, -1) [align=center] {Alcohol\\consumption};
\node (t) at (1.9,-1.5) [align=center] {Time of day};
\foreach \from/\to in {x/ms, g/x, mt/mp, u/mp, y/mp, no/mp, y/mp, w/mp, a/mp, st/x, ms/mp, a/ms, t/ms, t/mp}
  \draw [->] (\from) -- (\to);
\draw [->] (st.east) to (mp.north west);
\draw [->] (g.east) to [bend right=35] (mp.south west);
\end{tikzpicture}

