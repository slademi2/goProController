
(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 25.2.2017 *)

(*BeginPackage["goProHero3`"]*)
BeginPackage["GoProController`"]
(* Exported symbols added here with SymbolName::usage *) 

goProPrintModel::usage=
	"goProPrintModel[ ] returns string containing camera model."

Begin["`Private`"]

model="Hero3+Black";
goProPrintModel[]:=model


End[]

EndPackage[]