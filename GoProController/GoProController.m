(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 25.2.2017 *)

BeginPackage["GoProController`"]
(* Exported symbols added here with SymbolName::usage *) 

goProSetCamera::usage=
	"goProSetCamera[ _String] sets camera model to parameter given."
goProGetPossibleCamera::usage=
	"goProGetPossibleCamera[ ] returns possible cameras to control."

Begin["`Private`"]
(* Implementation of the package *)

cameraPossible={"Hero3+","Hero4"}


camera="";
(*goProSetCamera[param_String]:=(model=param;If[param=="Hero3+Black",Get["GoProController`goProHero3PlusBlack`"],Get["GoProController`Package`"]])*)
goProSetCamera[param_String]:=(camera=param;Switch[param,"Hero3+Black",Get["GoProController`goProHero3PlusBlack`"],
	"Hero4",(Get["GoProController`goProHero4`"])
	])
goProGetPossibleCamera[]:=cameraPossible





End[]

EndPackage[]

