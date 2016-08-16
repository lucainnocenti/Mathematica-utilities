(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



BeginPackage["utilities`"]


(* MONITORING UTILITIES *)
progressBar::usage="progressBar[n] displays a progress bar with progress n%.";
conditionalMonitor::usage="conditionalMonitor[test,i] gives a function which monitors the function i in its argument if test evaluated to True.";

(* CLEAR MEMOIZED VALUES UTILITIES *)
ClearDownValues::usage="ClearDownValues[p] removes the downvalues given in input as a pattern."
ClearCache::usage="ClearCache[f] clears all memoized values from f."

(* PLOTTING UTILITIES *)
simpleLegend
legendMaker::usage="Create a Graphics object with legends given by the list passed as the first argument. The options specify any non-deafult line styles (using PlotStyle -> {...}) or plot markers (using PlotMarkers -> {...}). For more options, inspect Options[legendMaker]";
extractStyles::usage="returns a tuple {\"all line style directives\", \"all plot markers\"} found in the plot, in the order they are drawn. The two sublists need not have the same length if some lines don't use markers ";
autoLegend::usage="Simplified legending for the plot passed as first argument, with legends given as second argument. Use the option Alignment -> {horizontal, vertical} to place the legend in the PlotRegion in scaled coordinates. For other options, see Options[legendMaker] which are used by autoLegend.";


exportHere::usage="exportHere[fileName,object] saves object in the current directory with the name fileName."
absSquared::usage="absSquared[z] returns the square of the absolute value of z."
traceView2::usage="traceView2[expr] returns a formatted view of the TraceScan of the expression."
seeOptions::usage="seeOptions[function] returns the list of options defined for function, without the clumsy whatever`private`stuff notation."
factorInteger::usage="factorInteger[n] returns the prime integer factorization of n, nicely formatted."
dynamicPlot
dynamicPlot2
texStyles


splineCircle::usage="splineCircle[c,r,angles] produces a BSplineCurve object representing an arc on the x-y plane, centered at the (2- or 3-dimensional) point m, with radius r, covering the angles specified in angles";


Begin["`Private`"]


factorInteger[n_Integer]:=FactorInteger[n]//Map[Superscript[#[[1]],#[[2]]]&]//Row[#,"\[Cross]"]&


progressBar[progressIndex_?NumericQ]:=Row[{ProgressIndicator[#,{0,100}],#}," "]&@progressIndex;

Attributes[conditionalMonitor]={HoldRest};
conditionalMonitor[test_,monitoredVariable_]:=Function[
code,
If[TrueQ@test,Monitor[code,progressBar[monitoredVariable]],code],
HoldAll
]

SetAttributes[ClearDownValues,HoldAllComplete];
ClearDownValues[p:f_[___]]:=
DownValues[f]=DeleteCases[
DownValues[f,Sort->False],
HoldPattern[Verbatim[HoldPattern][p]:>_]
];
ClearCache[f_]:=DownValues[f]=DeleteCases[DownValues[f],_?(FreeQ[First[#],Pattern]&)]


exportHere[name_,obj_]:=Export[FileNameJoin@{NotebookDirectory[],name},obj]


texStyles=<|
"plain"->Directive[Black,{FontFamily->"LM Roman 12",FontSize->24,FontWeight->Plain,FontSlant->Plain}],
"writing"->Directive[Black,{FontFamily->"CMU Serif",FontSize->20,FontWeight->Plain,FontSlant->Plain}],
"title"->Directive[Black,{FontFamily->"CMU Serif",FontSize->24,FontWeight->Plain,FontSlant->Plain}],
"labels"->Directive[Black,{FontFamily->"LMMathItalic10",FontSize->24,FontWeight->Plain,FontSlant->Plain}]
|>




seeOptions[function_]:=Thread@Rule[
#[[All,1]]//Map[SymbolName],
#[[All,2]]
]&@Options[function]


ClearAll[simpleLegend]
Options[simpleLegend]={fontSize->11,imageSize->{20,10},insetpts->10,lineThickness->0.001};
simpleLegend[legendItems__,pos_,opts:OptionsPattern[]]:=Module[{legendLine,offset,legend},
offset=Module[{s,o},
s=pos/.{Left->0,Right->1,Bottom->0,Top->1};
o=OptionValue@insetpts pos/.{Left->1,Right->-1,Bottom->1,Top->-1};
Offset[o,Scaled[s]]
];
legendLine[{lbl_,lineStyle_}]:={
Graphics[{lineStyle,Thickness@0.02,Line[{{0,0.5},{1,0.5}}]},ImageSize->OptionValue@imageSize,AspectRatio->0.5],
Style[lbl,FontFamily->"Tahoma",FontSize->OptionValue@fontSize,TextAlignment->Left,LineBreakWithin->False]
};
legend=GraphicsGrid[legendLine/@legendItems,Alignment->Left];
Graphics@Inset[Panel[legend],offset,pos]
];


Options[legendMaker]=Join[
FilterRules[Options[Framed],Except[{ImageSize,FrameStyle,Background,RoundingRadius,ImageMargins}]],
{
FrameStyle->None,
Background->Directive[Opacity[.7],LightGray],
RoundingRadius->10,
ImageMargins->0,
PlotStyle->Automatic,
PlotMarkers->None,
"LegendLineWidth"->35,
"LegendLineAspectRatio"->.3,
"LegendMarkerSize"->8,
"LegendGridOptions"->{Alignment->Left,Spacings->{.4,.1}}
}
];
legendMaker[textLabels_,opts:OptionsPattern[]]:=Module[{f,lineDirectives,markerSymbols,n=Length[textLabels],x},
lineDirectives=(
(PlotStyle/.{opts})/.PlotStyle|Automatic:>Map[ColorData[1],Range[n]]
)/.None->{None};
markerSymbols=Replace[
(
(PlotMarkers/.{opts})/.Automatic:>(Drop[Normal[ListPlot[Transpose[{Range[3]}],PlotMarkers->Automatic][[1,2]]][[1]],-1]/.Inset[x_,i__]:>x)[[All,-1]]
)/.{Graphics[gr__],sc_}:>Graphics[gr,ImageSize->("LegendMarkerSize"/.{opts}/.Options[legendMaker,"LegendMarkerSize"]/.{"LegendMarkerSize"->8})],
PlotMarkers|None:>Map[Style["",Opacity[0]]&,textLabels]]/.None|{}->Style["",Opacity[0]];
lineDirectives=PadRight[lineDirectives,n,lineDirectives];
markerSymbols=PadRight[markerSymbols,n,markerSymbols];
f=Grid[MapThread[{Graphics[{#1/.None->{},If[#1==={None}||(PlotStyle/.{opts})===None,{},Line[{{-.1,0},{.1,0}}]],Inset[#2,{0,0},Background->None]},AspectRatio->("LegendLineAspectRatio"/.{opts}/.Options[legendMaker,"LegendLineAspectRatio"]/.{"LegendLineAspectRatio"->.2}),ImageSize->("LegendLineWidth"/.{opts}/.Options[legendMaker,"LegendLineWidth"]/.{"LegendLineWidth"->35}),ImagePadding->{{1,1},{0,0}}],Text[#3,FormatType->TraditionalForm]}&,{lineDirectives,markerSymbols,textLabels}],Sequence@Evaluate[("LegendGridOptions"/.{opts}/.Options[legendMaker,"LegendGridOptions"]/.{"LegendGridOptions"->{Alignment->Left,Spacings->{.4,.1}}})]];
Framed[f,FilterRules[{Sequence[opts,Options[legendMaker]]},FilterRules[Options[Framed],Except[ImageSize]]]]];


extractStyles[plot_]:=Module[{lines,markers,points,extract=First[Normal[plot]]},(*In a plot,the list of lines contains no insets,so I use this to find it:*)lines=Select[Cases[Normal[plot],{___,_Line,___},Infinity],FreeQ[#1,Inset]&];
points=Select[Cases[Normal[plot],{___,_Point,___},Infinity],FreeQ[#1,Inset]&];
(*Most plot markers are inside Inset,except for Point in list plots:*)markers=Select[extract,!FreeQ[#1,Inset]&];
(*The function returns a list of lists:*){(*The first return value is the list of line plot styles:*)Replace[Cases[lines,{c__,Line[__],___}:>Flatten[Directive@@Cases[{c},Except[_Line]]],Infinity],{}->None],(*Second return value:marker symbols*)Replace[Join[Cases[markers,{c__,Inset[s_,pos_,d___],e___}:>If[(*markers "s" can be strings or graphics*)Head[s]===Graphics,(*Append scale factor in case it's needed later;
default 0.01*){s,Last[{.01,d}]/.Scaled[f_]:>First[f]},If[(*For strings,add line color if no color specified via text styles:*)FreeQ[s,CMYKColor|RGBColor|GrayLevel|Hue],Style[s,c],s]],Infinity],(*Filter out Pointsize-legends don't need it:*)Cases[points,{c___,Point[pt__],___}:>{Graphics[{c,Point[{0,0}]}]/.PointSize[_]:>PointSize[1],.01},Infinity]],{}->None]}]

Options[autoLegend]=Join[{Alignment->{Right,Top},Background->White,AspectRatio->Automatic},FilterRules[Options[legendMaker],Except[Alignment|Background|AspectRatio]]];
autoLegend[plot_Graphics,labels_,opts:OptionsPattern[]]:=Module[{lines,markers,align=OptionValue[Alignment]},{lines,markers}=extractStyles[plot];
Graphics[{Inset[plot,{-1,-1},{Left,Bottom},Scaled[1]],Inset[legendMaker[labels,PlotStyle->lines,PlotMarkers->markers,Sequence@@FilterRules[{opts},FilterRules[Options[legendMaker],Except[Alignment]]]],align,Map[If[NumericQ[#],Center,#]&,align]]},PlotRange->{{-1,1},{-1,1}},AspectRatio->(OptionValue[AspectRatio]/.Automatic:>(AspectRatio/.Options[plot,AspectRatio])/.Automatic:>(AspectRatio/.AbsoluteOptions[plot,AspectRatio]))]]


SetAttributes[dynamicPlot,HoldAll]

Options[dynamicPlot]=Join[Options[Plot],Options[Dynamic]];

dynamicPlot[f_,{x_Symbol,x0_,x1_},opts:OptionsPattern[]]:=
Module[{bag=Internal`Bag[]},
PrintTemporary@Dynamic[
ListPlot[Sort@Internal`BagPart[bag,All],
Evaluate[Sequence@@FilterRules[{opts},Options[ListPlot]]]
],
Evaluate[Sequence@@FilterRules[{opts},Options[Dynamic]]],
UpdateInterval->0.5
];
Plot[f[x],{x,x0,x1},
EvaluationMonitor:>(Internal`StuffBag[bag,{x,f[x]}]),
Evaluate[Sequence@@FilterRules[{opts},Options[Plot]]]
]
]

SetAttributes[dynamicPlot2,HoldAll]
Options[dynamicPlot2]=Join[Options[Plot],Options[ListPlot]];
dynamicPlot2[f_,{x_Symbol,x0_,x1_},opts:OptionsPattern[]]:=Module[
{bag=Internal`Bag[],a=False},
Plot[f,{x,x0,x1},
EvaluationMonitor:>(Internal`StuffBag[bag,{x,f}];a=!a),
Evaluate[Sequence@@FilterRules[{opts},Options[Plot]]]
]~Monitor~ListPlot[
(a;Sort@Internal`BagPart[bag,All]),
Evaluate[Sequence@@FilterRules[{opts},Options[ListPlot]]]
]
]


Options[barChart3D]={graphicsOptions->{},colorData->"TemperatureMap",preOptions->{}};
barChart3D[matrix_,OptionsPattern[]]:=Graphics3D[
{
Evaluate[OptionValue@preOptions],
Table[
{
If[!NumericQ[matrix[[i,j]]],{},
 Sequence@@{
ColorData[OptionValue@colorData][matrix[[i,j]]],
Cuboid[{i-1/2,j-1/2,0},{i+1/2,j+1/2,matrix[[i,j]]}]
}
]
},
{i,Length@matrix},
{j,Length@matrix[[1,All]]
}
]
},
Evaluate[OptionValue@graphicsOptions]
]


SetAttributes[absSquared,{NumericFunction,Listable}];
absSquared[z_]:=z Conjugate[z]


ClearAll@traceView2
traceView2[expr_]:=Module[{steps={},stack={},pre,post,show,dynamic},
pre[e_]:=(
stack={steps,stack};
steps={}
);
post[e_,r_]:=(
steps=First@stack~Join~{show[e,HoldForm[r],steps]};
stack=stack[[2]]
);
SetAttributes[post,HoldAllComplete];
show[e_,r_,steps_]:=Grid[
steps/.{
{}->{{
"Expr  ",
Row[{e," ",Style["inert",{Italic,Small}]}]
}},
_->{
{"Expr  ",e},
{"Steps",steps/.{
{}->Style["no definitions apply",Italic],
_:>OpenerView[{Length@steps,dynamic@Column[steps]}]
}
},
{"Result",r}
}
},
Alignment->Left,
Frame->All,
Background->{{LightCyan},None}
];
TraceScan[pre,expr,___,post];
Deploy@Pane[steps[[1]]/.dynamic->Dynamic,ImageSize->10000]
]
SetAttributes[traceView2,{HoldAllComplete}]


ClearAll[splineCircle];
splineCircle[m_List,r_,angles_List: {0,2 \[Pi]}]:=Module[{seg,\[Phi],start,end,pts,w,k},
{start,end}=Mod[angles//N,2 \[Pi]];
If[end<=start,end+=2 \[Pi]];
seg=Quotient[end-start//N,\[Pi]/2];
\[Phi]=Mod[end-start//N,\[Pi]/2];
If[seg==4,seg=3;\[Phi]=\[Pi]/2];
pts=r RotationMatrix[start].#&/@Join[
Take[{{1,0},{1,1},{0,1},{-1,1},{-1,0},{-1,-1},{0,-1}},2 seg+1],
RotationMatrix[seg \[Pi]/2].#&/@{{1,Tan[\[Phi]/2]},{Cos[\[Phi]],Sin[\[Phi]]}}
];
If[Length[m]==2,
pts=m+#&/@pts,
pts=m+#&/@Transpose@Append[Transpose@pts,ConstantArray[0,Length@pts]]
];
w=Join[
Take[{1,1/Sqrt[2],1,1/Sqrt[2],1,1/Sqrt[2],1},2 seg+1],
{Cos[\[Phi]/2],1}
];
k=Join[{0,0,0},Riffle[#,#]&@Range[seg+1],{seg+1}];
BSplineCurve[pts,SplineDegree->2,SplineKnots->k,SplineWeights->w]
]/;Length[m]==2||Length[m]==3


End[]
EndPackage[]
