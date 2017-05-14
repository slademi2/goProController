(* Wolfram Language Package *)
(* Created by the Wolfram Workbench 25.2.2017 *)

(*This application was developed as a bachelor thesis on Czech Technical University in Prague
 on Faculty of Information Technology. 
 
 It was also created with personal effort and  great help of project "goprowifihack" available on https://github.com/KonradIT/goprowifihack 
 mainly for specifying parameters of url which controls GoPro camera. In the project goprowifihack you can learn how the GoPro cameras are controlled.
 *)
 

(*The application is situated in more source files. 
Camera generations are separated to source files based on structure of url address which controls them.
*)



BeginPackage["GoProController`"]
(* Exported symbols added here with SymbolName::usage *) 

(*In this part of source file the public functions will be declared*)

goProSetCamera::usage=
	"goProSetCamera[ _String] sets camera model to parameter given."
goProGetPossibleCamera::usage=
	"goProGetPossibleCamera[ ] returns possible cameras to control."

Begin["`Private`"]

(* Implementation of the package *)

cameraPossible={"HERO3+","HERO4","HERO2","HERO3"}


(*variable which represents camera generation such as HERO2, HERO3, HERO3+, HERO4*)
camera="";


goProSetCamera::cameraNotSet="`1` No such camera supported. You have to set valid camera. See goProGetPossibleCamera to get list of camera supported."

(*sets camera generation to parameter given and include the next source file based on parameter*)
goProSetCamera[param_String]:=(camera=param;Switch[param, 
	"HERO2",Get["GoProController`goProHero3`"],
	"HERO3",Get["GoProController`goProHero3`"],
	"HERO3+",Get["GoProController`goProHero3`"],	
	"HERO4",Get["GoProController`goProHero4`"],
	_, Message[goProSetCamera::cameraNotSet, param];""
	]
	)

(*returns posssible parameters for goProSetCamera*)
goProGetPossibleCamera[]:=cameraPossible





End[]

EndPackage[]

