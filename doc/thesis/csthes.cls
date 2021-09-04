%
% This is file 'csthes.cls'
%
% Author: Bettina Finzel (bettina.finzel@uni-bamberg.de) and Michael Siebers (michael.siebers@uni-bamberg.de)
%
%
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{csthes}[2019/05/21 thesis class from CogSys-Group at University of Bamberg]

\RequirePackage{ifthen}

% Defaults / Base commands for options
\newcommand*{\@dateprefix}{}
\newcommand*{\selectedlanguage}{}
\newcommand*{\selectedtype}{}
\newcommand*{\@cosupervisor}{}
\newboolean{isThesis}
\setboolean{isThesis}{false}

% Declaring Options
\DeclareOption{draft}{%
	\renewcommand*{\@dateprefix}{\dateprefix\ }%
	\PassOptionsToClass{draft}{report}%
}
\DeclareOption{final}{%
	\renewcommand*{\@dateprefix}{\relax}%
	\PassOptionsToClass{final}{report}%
}
\DeclareOption{thesis}{%
	\renewcommand*{\selectedtype}{thesis}%
	\setboolean{isThesis}{true}
}
\ExecuteOptions{final,thesis}
\ProcessOptions\relax

\PassOptionsToPackage{final}{hyperref}% Always use hyperlinks

% Load base class 

% For onesided printing uncomment next command
\LoadClass[12pt,a4paper,oneside]{report}

% Load required Packages
\RequirePackage{natbib}
\RequirePackage{fancyhdr}
\RequirePackage[OT1]{fontenc}

% Define pagestyles
\fancypagestyle{plain}{
	\fancyhead{}% clear all header fields
	\fancyfoot{}% clear all footer fields
	\fancyfoot[LE,RO]{\thepage}%
	\renewcommand{\headrulewidth}{0pt}%
	\renewcommand{\footrulewidth}{0pt}%
}
\fancypagestyle{default}{
  \fancyhf{}
  \fancyhead[C]{}
  \fancyhead[R]{UPC MIRI}
  \fancyhead[L]{Master Thesis}
  \fancyfoot[L,C]{}
  \fancyfoot[R]{Page \thepage{} of \pageref{LastPage}}
  \setlength{\headheight}{15pt}
  \renewcommand{\headrulewidth}{0.4pt}
  \renewcommand{\footrulewidth}{0.4pt}
}
\fancypagestyle{blank}{
	\fancyhead{}% clear all header fields
	\fancyfoot{}% clear all footer fields
	\renewcommand{\headrulewidth}{0pt}%
	\renewcommand{\footrulewidth}{0pt}%
}

% Use blank pagestyle for empty pages inserted by \cleardoublepage
\def\cleardoublepage{\clearpage\if@twoside \ifodd\c@page\else
    \thispagestyle{blank}\hbox{}\newpage\if@twocolumn\hbox{}\newpage\fi\fi\fi}

\widowpenalty=10000
\clubpenalty=10000

% Define commands for title page

\newcommand{\subtitle}[1]{\renewcommand{\@subtitle}{#1}}
\newcommand{\@subtitle}{\relax}
\newcommand*{\setthesistype}[1]{\renewcommand*{\thesistype}{#1}}
\newcommand*{\thesistype}{\@latex@warning@no@line{No \protect\thesistype\space given}Enter the thesis type!}
\newcommand*{\thesistitle}{\@latex@warning@no@line{No \protect\thesistitle\space given}Enter Bachelor Thesis or Master Thesis!}
\newcommand*{\degree}[1]{\renewcommand*{\@degree}{#1}}
\newcommand*{\@degree}{\@latex@warning@no@line{No \protect\semester\space given}Enter the degree!}
\newcommand*{\supervisor}[1]{\renewcommand*{\@supervisor}{#1}}
\newcommand*{\@supervisor}{\@latex@warning@no@line{No \protect\supervisor\space given}Enter the supervisor!}
\newcommand*{\cosupervisor}[1]{\renewcommand*{\@cosupervisor}{#1}}

\newcommand*{\@@date}{\@dateprefix\@date}

\renewcommand\maketitle{\newpage%
  \let\oldand\and
  \thispagestyle{empty}
  \setcounter{page}\@ne
  \let\footnotesize\small
  \let\footnoterule\relax
  \let \footnote \thanks
  \begin{center}
    \vspace*{.02\textheight}
    \includegraphics[width=0.7\textwidth]{images/upc_logo.png}
  \end{center}
  \null\vfil  
  
%  \vskip 15\p@
  \begin{center}%
    {\Huge \@title \par}%
    {\huge \@subtitle \par}%
    \vskip 2.5em%
	{\Large \textsc{\thesistype} \textsc{Thesis} }%
	\vskip 2.5em%
	{\Large \textsc{\@degree} \par}  
	\vskip 0.5em%
	\Large
  	\vskip 0.5em%
    {\Large \@author \par}% 
    \vskip 2.5em%
    {\large \@@date \par}%       % Set date in \large size.
  \vfil
  {\large Supervisor/a : \@supervisor}
  \vskip 0.1em%
  {\large Co-Supervisor/a : \@cosupervisor}
  % TODO: Add cooperation partners here
  \end{center}\par
  \newpage
  \thispagestyle{empty}
  \vfil
  \setcounter{page}\@ne
  \setcounter{footnote}{0}%
  \global\let\maketitle\relax
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\@title\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
  \let\and\oldand
}

% Define structuring commands
\newcommand{\mainmatter}{%
  \cleardoublepage%
  \pagenumbering{arabic}%
  \pagestyle{default}%
  \setcounter{page}{1}%
}

\pagestyle{default}
