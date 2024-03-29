(* ::Package:: *)

BeginPackage["utilities`"]

Unprotect @@ Names["utilities`*"];
ClearAll @@ Names["utilities`*"];

(* MONITORING UTILITIES *)
progressBar::usage = "progressBar[n] displays a progress bar with progress n%.";
conditionalMonitor::usage = "conditionalMonitor[test,i] gives a function which monitors the function i in its argument if test evaluated to True.";


(* CLEAR MEMOIZED VALUES UTILITIES *)
clearDownValues::usage = "ClearDownValues[p] removes the downvalues given in \
input as a pattern.";
ClearMemoizedValues::usage = "ClearMemoizedValues[f] clears all memoized values from f.";


(* PLOTTING UTILITIES *)
simpleLegend;
legendMaker::usage = "Create a Graphics object with legends given by the list passed as the first argument. The options specify any non-deafult line styles (using PlotStyle -> {...}) or plot markers (using PlotMarkers -> {...}). For more options, inspect Options[legendMaker]";
extractStyles::usage = "returns a tuple {\"all line style directives\", \"all plot markers\"} found in the plot, in the order they are drawn. The two sublists need not have the same length if some lines don't use markers ";
autoLegend::usage = "Simplified legending for the plot passed as first argument, with legends given as second argument. Use the option Alignment -> {horizontal, vertical} to place the legend in the PlotRegion in scaled coordinates. For other options, see Options[legendMaker] which are used by autoLegend.";


(* Saving utilities *)
exportHere::usage = "exportHere[fileName, object] saves object in the current directory with the name fileName.";


(* Visualization manipulation *)
absSquared::usage = "absSquared[z] returns the square of the absolute value of z.";
absArgForm::usage = "absArgForm[z] returns the complex number z in |z| Exp[I Arg[z]] form, with the argument always between 0 and Pi.";
allReal::usage = "allReal[expr] removes all Conjugate wrapper (assuming everything is real).";
factorInteger::usage = "factorInteger[n] returns the prime integer factorization of n, nicely formatted.";


(* Advanced utilities *)
traceView::usage = "traceView[expr] returns a formatted view of the TraceScan of the expression.";
seeOptions::usage = "seeOptions[function] returns the list of options defined for function, without the clumsy whatever`private`stuff notation.";


(* Cool stuff *)
dynamicPlot;
dynamicPlot2;


(* Tex styles *)
texStyles;


(* Useful shortcuts *)
MF::usage = "MF[arg] is just an alias for MatrixForm[arg].";
FF::usage = "FF[expr] is just an alias for FullForm[expr].";


dynamicallyHighlight::usage = "DynamicallyHighlight[tableToHighlight,equivalenceTest] prints a dynamic version of tableToHighlight where hovering with the mouse on the element elem1 highlights all elements elem2 of tableToHighlight such that equivalenceTest[elem1,elem2] is True.";
dynamicallyAddText;


(* Graphics utilities *)
gridLines;
MP;
dynamicMatrixPlot;

(* 3D Plotting utilities *)
barChart3D::usage = "barChart3D[matrix_] draws a 3D plot chart, using the values of the input matrix as heights of the cuboids.";
splineCircle::usage = "splineCircle[c,r,angles] produces a BSplineCurve object representing an arc on the x-y plane, centered at the (2- or 3-dimensional) point m, with radius r, covering the angles specified in angles";
fromSphericalCoordinates::usage = "fromSphericalCoordinates[r, theta, phi] is a safer version of FromSphericalCoordinates which accepts more flexible angle ranges.";


(* Generation of random numbers *)
randomNormal::usage = "\
randomNormal[n] generates n normally distributed real numbers, with vanishing mean and unit variance.
randomNormal[\[Mu], \[Sigma], n] generates n normally distributed real numbers, with mean \[Mu] and RMS \[Sigma].";
randomComplexNormal::usage = "\
randomComplexNormal[n] generates n normally distributed complex numbers, with vanishing mean and unit variance.
randomComplexNormal[\[Mu], \[Sigma], n] generates n normally distributed complex numbers, with mean \[Mu] and RMS \[Sigma].";


(* Parameters extraction and replacing *)
extractParameters::usage = "extractParameters[expr] tries to extract all parameters of the form a[1], a[2], g[3] etc.";
replaceVars::usage = "replaceVars[][expr] replaces with random values all parameters extracted in expr through extractParameters.";


(* Interactive controllers *)
dynamicalSwitcher;

(* add new aliases *)
AddInputAlias;
AddInputAliases;

(* Other fun stuff *)
mouseHavingFun;


(* Evaluation handling *)
step;

(* Algebra utilities *)
symbolicNonCommutativeProduct::usage = "symbolicNonCommutativeProduct[expr] \
evaluates the input expression without assuming that the symbols commute. By \
default all (and only) the numerical values are treated as scalars, and all \
the symbols as non-commuting operators. The options \"Scalars\" and \"Additio\
nalOperatorRules\" can be used to override this assumption.";


(* Information theory utilities *)
shannonEntropy;
marginalProbabilityDistribution;
mutualInformation;
conditionalEntropyXcY;
conditionalEntropyYcX;


Begin["`Private`"];

MF[args___] := MatrixForm[Chop @ args];
FF[expr___] := FullForm[expr];

factorInteger[n_Integer] := FactorInteger[n] // Map[Superscript[#[[1]], #[[2]]]&] // Row[#, "\[Cross]"]&;

progressBar[progressIndex_?NumericQ] := Row[{ProgressIndicator[#, {0, 100}], #}, " "]&@progressIndex;

Attributes[conditionalMonitor] = {HoldRest};
conditionalMonitor[test_, monitoredVariable_] := Function[
  code,
  If[TrueQ@test, Monitor[code, progressBar[monitoredVariable]], code],
  HoldAll
]

SetAttributes[ClearDownValues, HoldAllComplete];
ClearDownValues[p : f_[___]] :=
    DownValues[f] = DeleteCases[
      DownValues[f, Sort -> False],
      HoldPattern[Verbatim[HoldPattern][p] :> _]
    ];
ClearMemoizedValues[f_] := DownValues[f] = DeleteCases[DownValues[f], _?(FreeQ[First[#], Pattern]&)];


exportHere[name_, obj_] := Export[FileNameJoin@{NotebookDirectory[], name}, obj];


texStyles = <|
  "plain" -> Directive[Black, {FontFamily -> "LM Roman 12", FontSize -> 24, FontWeight -> Plain, FontSlant -> Plain}],
  "writing" -> Directive[Black, {FontFamily -> "CMU Serif", FontSize -> 20, FontWeight -> Plain, FontSlant -> Plain}],
  "title" -> Directive[Black, {FontFamily -> "CMU Serif", FontSize -> 24, FontWeight -> Plain, FontSlant -> Plain}],
  "labels" -> Directive[Black, {FontFamily -> "LMMathItalic10", FontSize -> 24, FontWeight -> Plain, FontSlant -> Plain}]
|>;


seeOptions[function_] := Thread@Rule[
  #[[All, 1]] // Map[SymbolName],
  #[[All, 2]]
]&@Options[function];


ClearAll[simpleLegend];
Options[simpleLegend] = {fontSize -> 11, imageSize -> {20, 10}, insetpts -> 10, lineThickness -> 0.001};
simpleLegend[legendItems__, pos_, OptionsPattern[]] := Module[{legendLine, offset, legend},
  offset = Module[{s, o},
    s = pos /. {Left -> 0, Right -> 1, Bottom -> 0, Top -> 1};
    o = OptionValue@insetpts pos /. {Left -> 1, Right -> -1, Bottom -> 1, Top -> -1};
    Offset[o, Scaled[s]]
  ];
  legendLine[{lbl_, lineStyle_}] := {
    Graphics[{lineStyle, Thickness@0.02, Line[{{0, 0.5}, {1, 0.5}}]}, ImageSize -> OptionValue@imageSize, AspectRatio -> 0.5],
    Style[lbl, FontFamily -> "Tahoma", FontSize -> OptionValue@fontSize, TextAlignment -> Left, LineBreakWithin -> False]
  };
  legend = GraphicsGrid[legendLine /@ legendItems, Alignment -> Left];
  Graphics@Inset[Panel[legend], offset, pos]
];


Options[legendMaker] = Join[
  FilterRules[Options[Framed], Except[{ImageSize, FrameStyle, Background, RoundingRadius, ImageMargins}]],
  {
    FrameStyle -> None,
    Background -> Directive[Opacity[.7], LightGray],
    RoundingRadius -> 10,
    ImageMargins -> 0,
    PlotStyle -> Automatic,
    PlotMarkers -> None,
    "LegendLineWidth" -> 35,
    "LegendLineAspectRatio" -> .3,
    "LegendMarkerSize" -> 8,
    "LegendGridOptions" -> {Alignment -> Left, Spacings -> {.4, .1}}
  }
];
legendMaker[textLabels_, opts : OptionsPattern[]] := Module[{f, lineDirectives, markerSymbols, n = Length[textLabels], x},
  lineDirectives = (
    (PlotStyle /. {opts}) /. PlotStyle | Automatic :> Map[ColorData[1], Range[n]]
  ) /. None -> {None};
  markerSymbols = Replace[
    (
      (PlotMarkers /. {opts}) /. Automatic :> (Drop[Normal[ListPlot[Transpose[{Range[3]}], PlotMarkers -> Automatic][[1, 2]]][[1]], -1] /. Inset[x_, i__] :> x)[[All, -1]]
    ) /. {Graphics[gr__], sc_} :> Graphics[gr, ImageSize -> ("LegendMarkerSize" /. {opts} /. Options[legendMaker, "LegendMarkerSize"] /. {"LegendMarkerSize" -> 8})],
    PlotMarkers | None :> Map[Style["", Opacity[0]]&, textLabels]] /. None | {} -> Style["", Opacity[0]];
  lineDirectives = PadRight[lineDirectives, n, lineDirectives];
  markerSymbols = PadRight[markerSymbols, n, markerSymbols];
  f = Grid[MapThread[{Graphics[{#1 /. None -> {}, If[#1 === {None} || (PlotStyle /. {opts}) === None, {}, Line[{{-.1, 0}, {.1, 0}}]], Inset[#2, {0, 0}, Background -> None]}, AspectRatio -> ("LegendLineAspectRatio" /. {opts} /. Options[legendMaker, "LegendLineAspectRatio"] /. {"LegendLineAspectRatio" -> .2}), ImageSize -> ("LegendLineWidth" /. {opts} /. Options[legendMaker, "LegendLineWidth"] /. {"LegendLineWidth" -> 35}), ImagePadding -> {{1, 1}, {0, 0}}], Text[#3, FormatType -> TraditionalForm]}&, {lineDirectives, markerSymbols, textLabels}], Sequence@Evaluate[("LegendGridOptions" /. {opts} /. Options[legendMaker, "LegendGridOptions"] /. {"LegendGridOptions" -> {Alignment -> Left, Spacings -> {.4, .1}}})]];
  Framed[f, FilterRules[{Sequence[opts, Options[legendMaker]]}, FilterRules[Options[Framed], Except[ImageSize]]]]];


extractStyles[plot_] := Module[{lines, markers, points, extract = First[Normal[plot]]},
(*In a plot,the list of lines contains no insets,so I use this to find it:*)
  lines = Select[Cases[Normal[plot], {___, _Line, ___}, Infinity], FreeQ[#1, Inset]&];
  points = Select[Cases[Normal[plot], {___, _Point, ___}, Infinity], FreeQ[#1, Inset]&];
  (*Most plot markers are inside Inset,except for Point in list plots:*)
  markers = Select[extract, !FreeQ[#1, Inset]&];
  (*The function returns a list of lists:*)
  {
  (*The first return value is the list of line plot styles:*)
    Replace[Cases[lines, {c__, Line[__], ___} :> Flatten[Directive @@ Cases[{c}, Except[_Line]]], Infinity], {} -> None],
  (*Second return value:marker symbols*)
    Replace[
      Join[
        Cases[
          markers,
          {c__, Inset[s_, pos_, d___], e___} :> If[
          (*markers "s" can be strings or graphics*)
            Head[s] === Graphics,
          (*Append scale factor in case it's needed later; default 0.01*)
            {s, Last[{.01, d}] /. Scaled[f_] :> First[f]},
            If[
            (*For strings,add line color if no color specified via text styles:*)
              FreeQ[s, CMYKColor | RGBColor | GrayLevel | Hue],
              Style[s, c],
              s
            ]
          ],
          Infinity
        ],
      (*Filter out Pointsize-legends don't need it:*)
        Cases[
          points,
          {c___, Point[pt__], ___} :> {Graphics[{c, Point[{0, 0}]}] /. PointSize[_] :> PointSize[1], .01},
          Infinity
        ]
      ],
      {} -> None
    ]
  }
];

Options[autoLegend] = Join[{
  Alignment -> {Right, Top},
  Background -> White,
  AspectRatio -> Automatic
  },
  FilterRules[Options[legendMaker], Except[Alignment | Background | AspectRatio]]
];
autoLegend[plot_Graphics, labels_, opts : OptionsPattern[]] := Module[
  {lines, markers, align = OptionValue[Alignment]},
  {lines, markers} = extractStyles[plot];
  Graphics[{
      Inset[plot, {-1, -1}, {Left, Bottom}, Scaled[1]],
      Inset[
        legendMaker[labels,
          PlotStyle -> lines,
          PlotMarkers -> markers,
          Sequence @@ FilterRules[{opts},
            FilterRules[Options[legendMaker], Except[Alignment]]]
        ],
        align,
        Map[If[NumericQ[#], Center, #]&, align]
      ]
    },
    PlotRange -> {{-1, 1}, {-1, 1}},
    AspectRatio -> (OptionValue[AspectRatio] /. Automatic :> (AspectRatio /. Options[plot, AspectRatio]) /. Automatic :> (AspectRatio /. AbsoluteOptions[plot, AspectRatio]))
  ]
]


SetAttributes[dynamicPlot, HoldAll]
Options[dynamicPlot] = Join[Options[Plot], Options[Dynamic]];
dynamicPlot[f_, {x_Symbol, x0_, x1_}, opts : OptionsPattern[]] :=
  Module[{bag = Internal`Bag[]},
    PrintTemporary @ Dynamic[
      ListPlot[
        Sort @ Internal`BagPart[bag, All],
        Evaluate[Sequence @@ FilterRules[{opts}, Options[ListPlot]]]
      ],
      Evaluate[Sequence @@ FilterRules[{opts}, Options[Dynamic]]],
      UpdateInterval -> 0.01
    ];
    Plot[f, {x, x0, x1},
      EvaluationMonitor :> (Internal`StuffBag[bag, {x, f}]),
      Evaluate[Sequence @@ FilterRules[{opts}, Options[Plot]]]
    ]
  ];


Options[barChart3D] = {colorData -> "TemperatureMap", preOptions -> {}};
Options[barChart3D] = Join[Options[barChart3D], Options[Graphics3D]];
barChart3D[matrix_, opts : OptionsPattern[]] := With[
  {minmax = MinMax @ Flatten @ matrix},
  Graphics3D[
    {
      Evaluate[OptionValue[preOptions]],
      Table[
        {
          If[!NumericQ[matrix[[i, j]]], {},
            Sequence @@ {
              ColorData[OptionValue@colorData][(matrix[[i, j]] - First @ minmax) / (Last @ minmax - First @ minmax)],
              Cuboid[{i - 1 / 2, j - 1 / 2, 0}, {i + 1 / 2, j + 1 / 2, matrix[[i, j]]}]
            }
          ]
        },
        {i, Length@matrix},
        {j, Length@matrix[[1, All]]
        }
      ]
    },
    Axes -> True,
    BoxRatios -> {1, 1, 0.5},
    Evaluate[Sequence @@ FilterRules[{opts}, Options[Graphics3D]]]
  ]
];


gridLines[dim_Integer, gridSpacing_Integer] := {Black, FaceForm[],
  EdgeForm[{Thick, Black}],
  Rectangle @@ ## & /@ (
    {#, # + gridSpacing} & /@
      Tuples[#, {2}] & @ Range[0, dim - gridSpacing, gridSpacing]
  )
};


(* MP is just a shorthand for MatrixPlot, with some predefined commonly used
   options *)
Options @ MP = Options @ MatrixPlot;
MP[expr_, opts : OptionsPattern[]] := MatrixPlot[
  expr,
  Evaluate @ FilterRules[{opts}, Options @ MatrixPlot],
  PlotRangePadding -> None,
  Mesh -> All, MeshStyle -> Gray
];


(* ------ BEGIN dynamicMatrixPlot and utilities ------ *)

rowNumber[matrix_] := If[# === None, #,
  If[
    MatchQ[#, _Integer ? (1 <= # <= Length @ matrix &)],
    #, None
  ] & [
    1 + Length @ matrix - Last @ Floor[# + 1]
  ]
] & @ MousePosition["Graphics"];


colNumber[matrix_] := If[# === None, #,
  If[
    MatchQ[#, _Integer ? (1 <= # <= Last @ Dimensions @ matrix &)],
    #, None
  ] & [
    First @ Floor[# + 1]
  ]
] & @ MousePosition["Graphics"];


mouseOnGraphics[] := MousePosition["Graphics"] =!= None;


switchRows[matrix_, row1_, row2_] := ReplacePart[matrix,
  {row1 -> matrix[[row2]],
   row2 -> matrix[[row1]]}
];


switchCols[matrix_, col1_, col2_] := ReplacePart[matrix,
  {
    {row_, col1} :> matrix[[row, col2]],
    {row_, col2} :> matrix[[row, col1]]
  }
];


(* Replace neighboring rows or columns, conditionally to the value of `which`.
   The change is done in-place, that is, the value of `matrixVar` is changed
   directly. *)
Attributes @ switchNeighborsInPlace = HoldFirst;
switchNeighborsInPlace[
  matrixVar_, idx1_, idx2_, which : ("Rows" | "Cols")
] := Set[matrixVar,
  Which[
    which == "Cols",
    switchCols[matrixVar, idx1, idx2],
    which == "Rows",
    switchRows[matrixVar, idx1, idx2]
  ]
];


Attributes @ conditionallySwitchNeighbors = HoldAll;
conditionallySwitchNeighbors[
  matrix_, labelsList_, oldIdx_, newIdx_, which : ("Rows" | "Cols")
] := If[
  Abs[oldIdx - newIdx] == 1 // TrueQ,
  switchNeighborsInPlace[matrix, oldIdx, newIdx, which];
  labelsList[[{oldIdx, newIdx}]] = labelsList[[{newIdx, oldIdx}]];
  oldIdx = newIdx
];


drawHighlightingRectangles[len_, row_, col_] := {
  FaceForm[], EdgeForm[{Red, Thickness @ 0.01}],
  If[NumericQ @ row,
    Rectangle[{0, len - row}, {len, len - row + 1}],
    Sequence[]
  ],
  FaceForm[], EdgeForm[{Purple, Thickness @ 0.01}],
  If[NumericQ @ col,
    Rectangle[{col - 1, 0}, {col, len}],
    Sequence[]
  ]
};


makeFrameTicks[len_Integer, labels_List] := {
  {#, #}, {#, #}
} & @ Thread @ {Range @ len, labels};

makeFrameTicks[lenghts_List, labelsRows_List, labelsCols_List] := {
  {#1, #1}, {#2, #2}
} & [
  Thread @ {Range @ lenghts[[1]], labelsRows},
  Thread @ {Range @ lenghts[[2]], labelsCols}
];

makeFrameTicks[labels_List] := makeFrameTicks[Length @ labels, labels];
makeFrameTicks[labelsRows_List, labelsCols_List] := makeFrameTicks[
  Length /@ {labelsRows, labelsCols}, labelsRows, labelsCols
];


Attributes @ dynamicMatrixPlot = HoldFirst;

Options @ dynamicMatrixPlot = {"OverlayRectangles" -> False};

dynamicMatrixPlot[matrix_, opts : OptionsPattern[]] := If[
  (* If the input is directly given as a matrix, we make local variable holding
     it and recall the function with that variable as argument *)
  ! MatchQ[Hold @ matrix, Hold @ _Symbol],
  Module[{mat = matrix}, dynamicMatrixPlot[mat, opts]],

  With[{overlayRectangles = TrueQ @ OptionValue @ "OverlayRectangles"},
  DynamicModule[{
      len = Length @ matrix,
      oldRow, oldCol,
      rowsOrder = Range @ Length @ matrix,
      colsOrder = Range @ Length @ matrix
    },
    EventHandler[
      Dynamic @ MP[matrix,
        (* Epilog is used to draw the rectangles highlighting the row/column
           that is being moved *)
        If[overlayRectangles,
          Epilog -> drawHighlightingRectangles[len, oldRow, oldCol],
          Unevaluated @ Sequence[]
        ],
        FrameTicks -> makeFrameTicks @@ {rowsOrder, colsOrder}
      ],
      {
        "MouseDown" :> (
          Set[oldRow, rowNumber @ matrix];
          Set[oldCol, colNumber @ matrix]
        ),
        "MouseDragged" :> (
          If[And[
              mouseOnGraphics[],
              rowNumber @ matrix =!= None,
              colNumber @ matrix =!= None
            ],
            (* conditionallySwitchNeighbors handles the switching of neighboring
               rows and columns. Note that it also changed in-place `oldRow`
               and `oldCol`. *)
            conditionallySwitchNeighbors[
              matrix, rowsOrder, oldRow, rowNumber @ matrix, "Rows"
            ];
            conditionallySwitchNeighbors[
              matrix, colsOrder, oldCol, colNumber @ matrix, "Cols"
            ]
          ];
        )
      }
    ]
  ]]
];


(* ------ END dynamicMatrixPlot and utilities ------ *)


SetAttributes[absSquared, {NumericFunction, Listable}];
absSquared[z_] := z Conjugate[z];


absArgForm[z_Complex] := Which[
  Abs[z] == 0, 0,
  Arg[z] == 0, Abs[z],
  Abs[z] == 1,
  DisplayForm @ SuperscriptBox[
    "\[ExponentialE]", RowBox[{
      If[Arg[z] < 0, Pi + Arg[z], Arg[z]],
      "\[ImaginaryI]"
    }]
  ],
  True,
  DisplayForm @ RowBox[{
    Abs[z],
    DisplayForm @ SuperscriptBox[
      "\[ExponentialE]",
      RowBox[{
        Arg[z],
        "\[ImaginaryI]"
      }]
    ]
  }]
];
absArgForm[z_?NumericQ] := z;
absArgForm[expr_] := Replace[expr, z_Complex :> absArgForm[z], Infinity];

allReal[x___] := Replace[x, Conjugate[y_] :> y, Infinity];


ClearAll @ traceView;
SetAttributes[traceView, {HoldAllComplete}];
traceView[expr_] := Module[{steps = {}, stack = {}, pre, post, show, dynamic},
  pre[e_] := (
    stack = {steps, stack};
    steps = {}
  );
  post[e_, r_] := (
    steps = First@stack ~ Join ~ {show[e, HoldForm[r], steps]};
    stack = stack[[2]]
  );
  SetAttributes[post, HoldAllComplete];
  show[e_, r_, steps_] := Grid[
    steps /. {
      {} -> {{
        "Expr  ",
        Row[{e, " ", Style["inert", {Italic, Small}]}]
      }},
      _ -> {
        {"Expr  ", e},
        {"Steps", steps /. {
          {} -> Style["no definitions apply", Italic],
          _ :> OpenerView[{Length@steps, dynamic@Column[steps]}]
        }
        },
        {"Result", r}
      }
    },
    Alignment -> Left,
    Frame -> All,
    Background -> {{LightCyan}, None}
  ];
  TraceScan[pre, expr, ___, post];
  Deploy@Pane[steps[[1]] /. dynamic -> Dynamic, ImageSize -> 10000]
];


Options[dynamicallyHighlight] = {objectsToHighlight -> _};
SyntaxInformation[dynamicallyHighlight] = {"ArgumentsPattern" -> {_, _, OptionsPattern[]}};
(*DynamicallyHighlight[tableToPrint_,equivalenceTest_,OptionsPattern[]]:=DynamicModule[{rul=Identity},
EventHandler[
Dynamic[rul@#],
{"MouseMoved"\[RuleDelayed](rul=Function[y,If[equivalenceTest[#,y]||#\[Equal]y,Style[y,Red,Bold],y]])}
]&/@Cases[tableToPrint,OptionValue@objectsToHighlight]
];*)
dynamicallyHighlight[tableToPrint_, equivalenceTest_, OptionsPattern[]] := DynamicModule[{rul = Identity},
  tableToPrint /. obj : OptionValue@objectsToHighlight :> EventHandler[
    Dynamic[rul@obj],
    {"MouseMoved" :> (rul = Function[y,
      If[TrueQ[equivalenceTest[obj, y] || obj == y], Style[y, Red, Bold], y]
    ])}
  ]
];


ClearAll[dynamicallyAddText];
dynamicallyAddText[img_, text_] := DynamicModule[{pt = {0, 0}, fullImg},
  fullImg = Show[
    img,
    ImageSize -> Large,
    Epilog -> Dynamic@{
      Inset[text, pt, {-1, 0}]
    }
  ];
  Row[{
    fullImg,
    LocatorPane[Dynamic@pt,
      Show[img, ImageSize -> Medium]
    ],
    Button["Save image",
      CopyToClipboard@fullImg
    ]
  }]
];


ClearAll[splineCircle];
splineCircle[m_List, r_, angles_List : {0, 2 \[Pi]}] := Module[{seg, \[Phi], start, end, pts, w, k},
  {start, end} = Mod[angles // N, 2 \[Pi]];
  If[end <= start, end += 2 \[Pi]];
  seg = Quotient[end - start // N, \[Pi] / 2];
  \[Phi] = Mod[end - start // N, \[Pi] / 2];
  If[seg == 4, seg = 3;\[Phi] = \[Pi] / 2];
  pts = r RotationMatrix[start] . #& /@ Join[
    Take[{{1, 0}, {1, 1}, {0, 1}, {-1, 1}, {-1, 0}, {-1, -1}, {0, -1}}, 2 seg + 1],
    RotationMatrix[seg \[Pi] / 2] . #& /@ {{1, Tan[\[Phi] / 2]}, {Cos[\[Phi]], Sin[\[Phi]]}}
  ];
  If[Length[m] == 2,
    pts = m + #& /@ pts,
    pts = m + #& /@ Transpose@Append[Transpose@pts, ConstantArray[0, Length@pts]]
  ];
  w = Join[
    Take[{1, 1 / Sqrt[2], 1, 1 / Sqrt[2], 1, 1 / Sqrt[2], 1}, 2 seg + 1],
    {Cos[\[Phi] / 2], 1}
  ];
  k = Join[{0, 0, 0}, Riffle[#, #]&@Range[seg + 1], {seg + 1}];
  BSplineCurve[pts, SplineDegree -> 2, SplineKnots -> k, SplineWeights -> w]
] /; Length[m] == 2 || Length[m] == 3;

(*
  Do the same job as FromSphericalCoordinates, but being more flexible with the 
  allowed values of the angles. For example, don't throw an error for theta==Pi or
  theta > Pi.
*)
fromSphericalCoordinates[{r_, theta_, phi_}] := Which[
  r == 0, {0, 0, 0},
  theta == 0, {0, 0, 1},
  theta == Pi, {0, 0, -1},
  Pi < theta < 2 Pi, fromSphericalCoordinates @ {r, 2 Pi - theta, -phi},
  phi > Pi, fromSphericalCoordinates @ {r, theta, phi - 2 Pi},
  phi <= -Pi, fromSphericalCoordinates @ {r, theta, phi + 2 Pi},
  True, FromSphericalCoordinates @ {r, theta, phi}
];
fromSphericalCoordinates[r_, theta_, phi_] := fromSphericalCoordinates@{r, theta, phi};
fromSphericalCoordinates[{theta_, phi_}] := fromSphericalCoordinates@{1, theta, phi};
fromSphericalCoordinates[theta_, phi_] := fromSphericalCoordinates@{1, theta, phi};


extractParameters[coeff_Symbol][expr_] := Union @ Cases[
  expr, coeff[__Integer], Infinity
];

extractParameters[expr_?(!AtomQ @ # &)] := Union[
  Cases[
    expr,
    Except[
      HoldPattern[Complex[__] | Rational[__]], _Symbol[__Integer]
    ],
    Infinity
  ]
];


replaceVars[symbols_ : None] = With[{
  pars = If[symbols === None,
    Echo[extractParameters[#], "Extracted parameters:"],
    symbols
  ]
},
  # /. Thread[
    pars -> RandomReal[{0, 1}, Length@pars]
  ]
] &;


(* Generation of random numbers *)
randomNormal[n_] := RandomVariate[NormalDistribution[], n];
randomNormal[\[Mu]_, \[Sigma]_, n_] := RandomVariate[NormalDistribution[\[Mu], \[Sigma]], n];

randomComplexNormal[n_Integer] := #[[All, 1]] + I #[[All, 2]] & @ RandomVariate[NormalDistribution[0, 1 / Sqrt@2], {n, 2}];
randomComplexNormal[ns : {__Integer}] := With[{alls = Sequence @@ ConstantArray[All, Length@ns]},
  #[[alls, 1]] + I #[[alls, 2]] & @ RandomVariate[NormalDistribution[0, 1 / Sqrt@2], Append[ns, 2]]
];
randomComplexNormal[\[Mu]_, \[Sigma]_, n_Integer] := Plus[
  \[Mu],
  #[[All, 1]] + I #[[All, 2]] & @
      RandomVariate[NormalDistribution[0, \[Sigma] / Sqrt@2], {n, 2}]
];
randomComplexNormal[\[Mu]_, \[Sigma]_, ns : {__Integer}] := Plus[
  \[Mu],
  With[{alls = Sequence @@ ConstantArray[All, Length@ns]},
    #[[alls, 1]] + I #[[alls, 2]] & @
        RandomVariate[NormalDistribution[0, \[Sigma] / Sqrt@2],
          Append[ns, 2]]
  ]
];


ClearAll @ dynamicalSwitcher;
Attributes[dynamicalSwitcher] = {HoldAll};
dynamicalSwitcher[positions_, opts : OptionsPattern[]] := With[
  {nSquares = Length@positions},
  DynamicModule[{selectedSquare = None},
    Graphics[{
      Dynamic@Table[
        {
          Hue[positions[[xPos]] / 6], EdgeForm@Black,
          Rectangle[{xPos - 1, 0}, {xPos, 1}],
          Black,
          Style[Text[positions[[xPos]], {xPos - 1 / 2, 1 / 2}],
            FontSize -> 20]
        },
        {xPos, Range@Length@positions}
      ],

    (* Draw partially opaque rectangle over selected square *)
      Dynamic@If[selectedSquare =!= None,
        {Gray, Opacity@0.2,
          Rectangle[{# - 1, 0} + .1, {#, 1} - .1]} &@selectedSquare,
        {}
      ],

    (* Draw invisible rectangles on all the squares, attaching
       to each one an EventHandler to manage the dynamics. *)
      Table[
        EventHandler[
          {Opacity@0, Rectangle[{xPos - 1, 0}, {xPos, 1}]},
          With[{xPos = xPos},
            {"MouseClicked" :> Which[
            (* Select the square if no other square was previously
               selected *)
              selectedSquare === None, selectedSquare = xPos,
            (* Otherwise, switch new square with the previously
               selected one, and than revert to initial condition *)
              selectedSquare != xPos,
              positions = ReplacePart[positions, {
                selectedSquare -> positions[[xPos]],
                xPos -> positions[[selectedSquare]]
              }];
              selectedSquare = None
            ]}
          ]
        ],
        {xPos, Length@positions}
      ]
    },
      Evaluate@FilterRules[{opts}, Options@Graphics],
      Frame -> False, FrameTicks -> None,
      ImageSize -> Large
    ]
  ]
];


AddInputAliases[rules_List] := SetOptions[
  EvaluationNotebook[],
  InputAliases -> DeleteDuplicates @ Join[rules,
    InputAliases /.
    Quiet[Options[EvaluationNotebook[], InputAliases]] /.
    InputAliases -> {}
  ]
];

AddInputAlias[rule_Rule] := AddInputAliases[{rule}];


(* Evaluation handling *)
SetAttributes[step, HoldAll];
step[expr_] := Module[{P},
  P = (P = Return[# /. HoldForm[x_] :> Defer[step[x]], TraceScan] &) &;
  TraceScan[P, expr, TraceDepth -> 1]
];


(* Mess a bit with the mouse *)
mouseHavingFun[] := (
  Needs["JLink`"];
  JLink`ReinstallJava[];
  robotclass = JLink`JavaNew["java.awt.Robot"];
  Table[
    robotclass @ mouseMove[Round[1000 (Cos[x] + 1)], Round[500 (Sin[2 x] + 1)]],
    {x, 0, 4 Pi, .001}
  ];
);


Attributes[makeAdditionalOperatorRules] = HoldAll;
makeAdditionalOperatorRules[args_List] := Block[{eqs, lhs, rhs, symbNCP},
  eqs = Sequence @@ Map[Hold, Hold @ args, {2}] // Evaluate;
  symbNCP = Function[Null,
    symbolicNonCommutativeProduct[#, "NonCommutativeProductWrapper" -> times],
    HoldAll
  ];
  Table[
    lhs = eq[[{1}, 1]] /. Hold[s__] :> symbNCP @ s;
    lhs = With[{lhsToInject = Sequence @@ lhs},
      eq[[{1}, 1]] /.
        Hold[__] :> Hold[times[left___, lhsToInject, right___]]
    ];

    rhs = eq[[{1}, 2]] /. Hold[s__] :> symbNCP @ s;
    rhs = With[{rhsToInject = rhs},
      eq[[{1}, 1]] /.
        Hold[__] :> Hold[times[left, rhsToInject, right]]
    ];
    (* output *)
    {lhs, rhs} /. {Hold[left__], Hold[right__]} :> (left :> right),
    (* iterators *)
    {eq, eqs}
  ]
];
makeAdditionalOperatorRules[Hold[args__]] :=
  makeAdditionalOperatorRules @ {args}

Attributes[symbolicNonCommutativeProduct] = HoldAll;
Options[symbolicNonCommutativeProduct] = {
  "Scalars" -> {},
  "ScalarsPattern" -> None,
  "AdditionalOperatorRules" -> {},
  "NonCommutativeProductWrapper" -> NonCommutativeMultiply
};
symbolicNonCommutativeProduct[expr_, OptionsPattern[]] := With[{
    exprNC = ReleaseHold[
        Hold[expr] /. {
          Times -> times,
          Power[s_, n_Integer /; n > 0] :> times @@ ConstantArray[s, n]
        }
      ],
    scalar = If[OptionValue @ "ScalarsPattern" =!= None,
      OptionValue @ "ScalarsPattern",
      _ ? (NumericQ @ # || MemberQ[OptionValue @ "Scalars", #] &)
    ],
    customRules = If[
      MatchQ[OptionValue @ "AdditionalOperatorRules", _Hold],
      makeAdditionalOperatorRules @
        Evaluate @ OptionValue @ "AdditionalOperatorRules",
      (* else.. *)
      {}
    ]
  },
  exprNC //. {
    (*s:_times/;(
    Print@Panel@Row@{"Current state: ",
    Style[s,Red]
    };False
    )\[RuleDelayed]Null,*)

    (* Distribute times over Plus *)
    times[left___, HoldPattern @ Plus[middle__], right___] :> (
      Plus @@ (times[left, #, right] & /@ {middle})
    ),

    (* Take scalars out of sum *)
    times[left___, (a : scalar) * middle___, right___] :>
      a times[left, middle, right],
    times[left___, a : scalar /; Head @ a =!= times, right___] :> Times[
      a,
      times[left, right]
    ],

    (* Simulate Flat and OneIdentity properties (kind of) *)
    times[s : (_Symbol | _?NumericQ | _times)] :> s,
    times[left___, times[middle___], right___] :> times[left, middle, right],
    times[] :> 1,

    (* Apply additional rules if specified *)
    Sequence @@ customRules
  } /. times -> OptionValue @ "NonCommutativeProductWrapper"
];


(* information theory utilities *)
shannonEntropy[probs_, base_:2] := DeleteCases[probs, _?PossibleZeroQ] // -Total[# * Log[2, #]] &;
marginalProbabilityDistribution[probs_List, sizes : {__Integer}, remainingDof_Integer] := With[
	{probsMatrix = ArrayReshape[probs, sizes]},
	Total[probsMatrix, Complement[Range @ Length @ sizes, {remainingDof}]]
];
mutualInformation[probs_List, sizes : {_Integer, _Integer} : {2, 2}] := Plus[
	shannonEntropy[marginalProbabilityDistribution[probs, sizes, 1]],
	shannonEntropy[marginalProbabilityDistribution[probs, sizes, 2]],
	- shannonEntropy @ Flatten @ probs
];
conditionalEntropyXcY[probs_List, sizes : {sizeX_Integer, sizeY_Integer} : {2, 2}] := Total @ With[
	{probsMat = ArrayReshape[probs, sizes]},
	Transpose @ probsMat // Map[Total @ # * shannonEntropy[# / Total @ #] &]
];
conditionalEntropyYcX[probs_List, sizes : {sizeX_Integer, sizeY_Integer} : {2, 2}] := Total @ With[
	{probsMat = ArrayReshape[probs, sizes]},
	probsMat // Map[Total @ # * shannonEntropy[# / Total @ #] &]
];


(* Protect all package symbols *)
With[{syms = Names["utilities`*"]},
  SetAttributes[syms, {Protected, ReadProtected}]
];

End[];
EndPackage[];
