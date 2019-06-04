(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["BasicQGRFunctions`"]


(* ::Input::Initialization:: *)
SaveToCell::usage=
"SaveToCell[variable] creates an input cell that reassigns the current value of variable.\n SaveToCell[variables,display] shows 'display' on the right-hand-side of the assignment.";


(* ::Input::Initialization:: *)
VectorTranslate::usage="VectorTranslate[vector1, vector2] translates by vector2.  VectorTranslate[array, vector] translates the vectors in the array.  VectorTranslate[vector] can be used to translate by vector.";


(* ::Input::Initialization:: *)
VectorProject::usage="VectorProject[array, matrix] uses the matrix as a projection on the vectors in the array.  VectorProject[matrix] can be used to do projections.";


(* ::Input::Initialization:: *)
QGRData::usage="QGRData[\"Entities\"]  lists available entities.  QGRData[\"Properties\"] lists the properties available.";


(* ::Input::Initialization:: *)
Begin["`Private`"]


(* ::Input::Initialization:: *)
SetAttributes[SaveToCell,HoldFirst];

SaveToCell[var_,name:Except[_?OptionQ]:"data",opt:OptionsPattern[]]:=
With[{
data=Compress[var],
panel=ToBoxes[Tooltip[Panel[name,FrameMargins->0],DateString[]]]},
CellPrint[
Cell[BoxData[RowBox[
{MakeBoxes[var],
"=",
InterpretationBox[panel,Uncompress[data]]
,";"
}]
]
,"Input"
,GeneratedCell->False,
CellLabel->"(saved)",
opt,
CellLabelAutoDelete->False]
]
]


(* ::Input::Initialization:: *)
VectorTranslate[{},\[DoubleStruckW]_]:={};
VectorTranslate[\[DoubleStruckCapitalV]_,{}]:={};
VectorTranslate[\[DoubleStruckV]_?VectorQ,\[DoubleStruckW]_?VectorQ]:=\[DoubleStruckV]+\[DoubleStruckW];
VectorTranslate[\[DoubleStruckCapitalV]_?MatrixQ,\[DoubleStruckW]_?VectorQ]:=Transpose[Transpose[\[DoubleStruckCapitalV]]+\[DoubleStruckW]];
VectorTranslate[\[DoubleStruckCapitalV]_?ArrayQ,\[DoubleStruckW]_?VectorQ]:=TransposeArray[TransposeArray[\[DoubleStruckCapitalV]]+\[DoubleStruckW]];
VectorTranslate[\[DoubleStruckCapitalV]_,\[DoubleStruckW]_?VectorQ]:=Map[VectorTranslate[#,\[DoubleStruckW]]&,\[DoubleStruckCapitalV]];
VectorTranslate[\[DoubleStruckCapitalV]_,\[DoubleStruckCapitalW]_]:=Map[VectorTranslate[\[DoubleStruckCapitalV],#]&,\[DoubleStruckCapitalW],{-2}];
VectorTranslate[\[DoubleStruckCapitalW]_][\[DoubleStruckCapitalV]_]:=VectorTranslate[\[DoubleStruckCapitalV],\[DoubleStruckCapitalW]];


(* ::Input::Initialization:: *)
TransposeArray[\[DoubleStruckCapitalV]_?ArrayQ]:=Transpose[\[DoubleStruckCapitalV],Cycles[{{ArrayDepth[\[DoubleStruckCapitalV]],1}}]]


(* ::Input::Initialization:: *)
VectorProject[\[DoubleStruckCapitalV]_?ArrayQ,subspace_?MatrixQ]:=\[DoubleStruckCapitalV].Transpose[subspace];
VectorProject[\[DoubleStruckCapitalV]_,subspace_?MatrixQ]:=Map[VectorProject[#,subspace]&][\[DoubleStruckCapitalV]];
VectorProject[subspace_][\[DoubleStruckCapitalV]_]:=VectorProject[\[DoubleStruckCapitalV],subspace];


(* ::Input::Initialization:: *)
QGRData["Entities"]:={{"Dn",Integer}};


(* ::Input::Initialization:: *)
QGRData["Properties"]:={"Roots",{"Layers",Integer},"MemberQ"};


(* ::Input::Initialization:: *)
QGRData[{"Dn",n_Integer},"Roots"]:=Table[Permutations[a*UnitVector[n,1]+b*UnitVector[n,2]],{a,{-1,1}},{b,{-1,1}}]//Flatten[#,2]&//Union


(* ::Input::Initialization:: *)
QGRData[{"Dn",n_Integer},{"Layers",layers_Integer}]:=Module[{\[CapitalLambda],\[ScriptCapitalR],\[CapitalLambda]\[Prime]},
\[CapitalLambda]={ConstantArray[0,n]};
\[ScriptCapitalR]=QGRData[{"Dn",n},"Roots"];
Do[
\[CapitalLambda]\[Prime]=\[CapitalLambda];
Do[\[CapitalLambda]=Union[\[CapitalLambda],VectorTranslate[\[CapitalLambda]\[Prime],\[ScriptR]]],{\[ScriptR],\[ScriptCapitalR]}];
,{layers}];
\[CapitalLambda]=\[CapitalLambda]//SortBy[Norm];
Return[\[CapitalLambda]];
]


(* ::Input::Initialization:: *)
QGRData[{"Dn",n_Integer},"MemberQ"]:=Map[Function[\[DoubleStruckV],AllTrue[\[DoubleStruckV],IntegerQ]&&EvenQ[Total[\[DoubleStruckV]]]],#,{ArrayDepth[#]-1}]&


(* ::Input::Initialization:: *)
End[]


(* ::Input::Initialization:: *)
EndPackage[]
