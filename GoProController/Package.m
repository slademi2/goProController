(* Wolfram Language Package *)

BeginPackage["GoProController`"]
(* Exported symbols added here with SymbolName::usage *)  
goProPrintModel::usage=
	"goProPrintModel[ ] returns string containing camera model."

goProShutter::usage=
	"goProShutter[ ] Start of recording or taking photo on camera"
	
	
Begin["`Private`"] (* Begin Private Context *) 

model="Hero4";
goProPrintModel[]:=model

goProShutter[]:="Package.m Trol"

End[] (* End Private Context *)

EndPackage[]