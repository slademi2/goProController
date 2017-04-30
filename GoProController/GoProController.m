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

cameraPossible={"HERO3+","HERO4","HERO2","HERO3"}



camera="";
goProSetCamera::cameraNotSet="`1` No such camera supported. You have to set valid camera. See goProGetPossibleCamera to get list of camera supported."
(*goProSetCamera[param_String]:=(model=param;If[param=="Hero3+Black",Get["GoProController`goProHero3PlusBlack`"],Get["GoProController`Package`"]])*)
goProSetCamera[param_String]:=(camera=param;Switch[param, 
	"HERO2",Get["GoProController`goProHero3`"],
	"HERO3",Get["GoProController`goProHero3`"],
	"HERO3+",Get["GoProController`goProHero3`"],	
	"HERO4",Get["GoProController`goProHero4`"],
	_, Message[goProSetCamera::cameraNotSet, param];""
	]
	)

goProGetPossibleCamera[]:=cameraPossible





End[]

EndPackage[]

