(* Wolfram Language Package *)

BeginPackage["GoProController`"]
(* Exported symbols added here with SymbolName::usage *)  


(* ::Section:: *)
(* Public Definitions *)
goProSetCameraModel::usage=
	"goProSetCameraModel[ _String] sets camera model to further specification.
	 To see which models of GoProHero4 are supported call goProGetPossibleCameraModel."	
goProGetPossibleCameraModel::usage=
	"goProGetPossibleCameraModel[] returns possible cameras model to control. Black, Silver or Session"
	
	
goProGetCamera::usage=
	"goProPrintModel[ ] returns string containing camera such as Hero4."
goProGetCameraModel::usage=
	"goProPrintModel[ ] returns string containing camera model such as Black edition."
	
(*debug*)
goProMakeCommand::usage=
	"debug to see command structure"

(*Shutter*)
goProShutter::usage =
    "goProShutter[ ] Start of recording or taking photo on camera"

goProStop::usage =
    "goProStop[ ] Stop the camera recording"
    

(* ::Subsection:: *)
(* WhiteBalance *)
(*White Balance*)
goProGetPossibleWhiteBalance::usage =
    "goProGetPossiblePhotoWhiteBalance[ ] returns possible values for Photo WhiteBalance"
goProSetPhotoWhiteBalance::usage =
    "goProSetPhotoWhiteBalance[ _String] sets Photo White Balance to parameter given.
     To see which parameters are supported call goProGetPossibleWhiteBalance[]."

goProSetVideoWhiteBalance::usage =
    "goProSetVideoWhiteBalance[ _String] sets Video White Balance to parameter given.
     To see which parameters are supported call goProGetPossibleWhiteBalance[]."
	
goProSetMultiShotWhiteBalance::usage =
    "goProSetMultiShotWhiteBalance[ _String] sets MultiShot White Balance to parameter given.
To see which parameters are supported call goProGetPossibleWhiteBalance[]."
	


(* ::Subsection:: *)
(* Color Profile *)
(*Color Profile*)
goProGetPossibleColorProfile::usage =
    "goProGetPossibleColors[ ] returns possible parameters for color profile setting"
goProSetVideoColorProfile::usage =
    "goProSetVideoColorProfile[ _String] sets color profile to parameter given. To see which parameters are supported call goProGetPossibleColorProfile[]."
goProSetPhotoColorProfile::usage =
    "goProSetPhotoColorProfile[ _String] sets color profile to parameter given. To see which parameters are supported call goProGetPossibleColorProfile[]."
goProSetMultiShotColorProfile::usage =
    "goProSetMultiShotColorProfile[ _String] sets color profile to parameter given. To see which parameters are supported call goProGetPossibleColorProfile[]."
   


(* ::Subsection:: *)
(* ISO Limit*)
goProGetPossibleVideoIsoLimit::usage =
    "goProGetPossibleVideoIsoLimit[ ] returns possible parameters for ISO limit settings."
goProGetPossiblePhotoIsoLimit::usage =
    "goProGetPossiblePhotoIsoLimit[ ] returns possible parameters for ISO limit settings."
goProGetPossibleMultiShotIsoLimit::usage =
    "goProGetPossibleMultiShotIsoLimit[ ] returns possible parameters for ISO limit settings."

goProSetVideoIsoLimit::usage =
    "goProSetVideoIsoLimit[ _String] sets ISO to parameter given. To see which parameters are supported call goProGetPossibleVideoIsoLimit[]."
goProSetPhotoIsoLimit::usage =
    "goProSetPhotoIsoLimit[ _String] sets ISO to parameter given. To see which parameters are supported call goProGetPossiblePhotoIsoLimit[]."
goProSetMultiShotIsoLimit::usage =
    "goProSetMultiShotIsoLimit[ _String] sets ISO to parameter given. To see which parameters are supported call goProGetPossibleMultiShotIsoLimit[]."
    
    

(* ::Subsection:: *)
(* Iso Mode *)
goProSetIsoMode::usage=
	"goProSetIsoMode[ _String] sets Iso Mode to parameter given. To see which parameter you can use call goProGetPossibleIsoMode[]."
goProGetPossibleIsoMode::usage=
	"goProGetPossibleIsoMode[] returns possible values for Iso Mode setting via goProSetIsoMode[_String]."

(* ::Subsection:: *)
(* Minimal Iso *)
goProSetPhotoIsoMinimum::usage=
	"goProSetPhotoIsiMinimum[ _String] sets Iso minimum for photo to value given. To see which value you can use call goProGetPossiblePhotoIsoMinimum[]. "
goProGetPossiblePhotoIsoMinimum::usage=
	"goProGetPossiblePhotoIsoMinimum[] returns values usable for goProSetPhotoIsiMinimum function."

goProSetMultiShotIsoMinimum::usage=
	"goProSetMultiShotIsiMinimum[ _String] sets Iso minimum for multi shot to value given. To see which value you can use call goProGetPossibleMultiShotIsoMinimum[]. "
goProGetPossibleMultiShotIsoMinimum::usage=
	"goProGetPossibleMultiShotIsoMinimum[] returns values usable for goProSetMultiShotIsiMinimum function."
	
	
	
	

(* ::Subsection:: *)
(* Sharpness *)
goProGetPossibleSharpness::usage=
	"goProGetPossibleSharpness[ ] returns usable values for sharpness setting. "

goProSetVideoSharpnes::usage=
	"goProSetVideoSharpnes[ _String] sets Video sharpness to parameter given, to see which parameter you can use call goProGetPossibleSharpness function."
	
goProSetPhotoSharpnes::usage=
	"goProSetPhotoSharpnes[ _String] sets Photo sharpness to parameter given, to see which parameter you can use call goProGetPossibleSharpness function."
	
goProSetMultiShotSharpnes::usage=
	"goProSetMultiShotSharpnes[ _String] sets MultiShot sharpness to parameter given, to see which parameter you can use call goProGetPossibleSharpness function."





(* ::Subsection:: *)
(* VIDEO Resolution *)
goProGetPossibleVideoResolution::usage=
	"goProGetPossibleVideoResolution[] returns possible parameters for video resolution setting. 
	Camera model must be set, to do so call goProSetCameraModel[ _String]"
goProSetVideoResolution::usage=
	"goProSetVideoResolution[_String] sets video resolution to parameter given.
	 To see which parameter you cen use call goProGetPossibleVideoResolution[] function.
	  Camera model must be set, to do so call goProSetCameraModel[ _String]"




(* ::Subsection:: *)
(* FPS *)
goProGetPossibleFPS::usage =
    "goProGetPossibleF[ ] Returns list of possible FPS."
(*goProGetPossibleFPS::usage =
    "goProGetPossibleFPS[ _String] Returns list of possible FPS for video resolution specified by parameter"*)
goProSetFPS::usage =
    "goProSetFPS[_String] Sets FPS to given parameter, but only if this amount of FPS is supported by actual video resolution.
	 To see which parameter you can use for actual video resolution use goProGetPossibleFPS functuon"
    



(* ::Subsection:: *)
(* FOV *)
goProSetFOV::usage =
    "goProSetFOV[_String ] Sets FOV to given parameter, accepts only possible parameters. 
	To see which parameter you can use for actual video resolution use goProGetPossibleFOV function" 

goProGetPossibleFOV::usage =
    "goProGetPossibleFOV[ ] Returns list of possible settings for FOV."



(* ::Subsection:: *)
(* Low Light *)
goProGetPossibleLowLight::usage =
    "goProGetPossibleLowLight[ ] returns possible parameters for LowLight settings"
goProSetLowLight::usage =
    "goProSetLowLight[ _String] sets LowLight to parameter given. To see which parameters are supported call goProGetPossibleLowLight[]."
goProSwitchLowLightOn::usage =
    "goProSwitchLowLightOn[ ] turns LowLight on."
goProSwitchLowLightOff::usage =
    "goProSwitchLowLightOff[ ] turns LowLight off."


(* ::Subsection:: *)
(* Spot Meter *)

goProGetPossibleSpotMeter::usage =
    "goProGetPossibleSpotMeter[ ] returns possible parameters for SpotMeter settings"
goProSetSpotMeter::usage =
    "goProSetSpotMeter[ _String] sets SpotMeter to parameter given. To see which parameters are supported call goProGetPossibleSpotMeter[]."
goProSwitchSpotMeterOn::usage =
    "goProSwitchSpotMeterOn[ ] turns SpotMeter on."
goProSwitchSpotMeterOff::usage =
    "goProSwitchSpotMeterOff[ ] turns SpotMeter off."







(* ::Section:: *)
(* Private Definitions *)	

Begin["`Private`"] (* Begin Private Context *) 

camera = "Hero4";
model="";

goProGetCamera[] := camera

modelPossible={"Black","Silver","Session"}
goProGetPossibleCameraModel[]:=modelPossible

goProSetCameraModel[param_String]:=model=param
goProGetCameraModel[]:=model;



goProUrl = "http://10.5.5.9/gp/gpControl/";

(*function for putting together url for GoProCommand,
 mode={setting,command}
*)
goProMakeCommand[mode_String,param1_String,param2_String]:= goProUrl <> mode <> "/" <> param1 <> "/" <> param2;

goProMakeCommand[mode_String,param_String] := goProUrl <> mode <> "/" <> param;

(*spusteni prikazu exec pomoci HTTPRequest a URLRead*)
execute[exec_String] := ((*request = HTTPRequest[exec];
     URLRead[request]*)exec)
 
 
 

(* ::Subsection:: *)
(* Shutter *)
goProShutter[] :=
    execute[goProMakeCommand["command","shutter?p=1"]]
goProStop[] :=
    execute[goProMakeCommand["command","shutter?p=0"]]
    


(* ::Subsection:: *)
(* WhiteBalance *)
wbPhoto="22"
wbVideo="11"
wbMulti="35"
wbToCode = <|
"auto"->"0",
"3000k"->"1",
"4000k"->"5",
"4800k"->"6",
"5500k"->"2",
"6000k"->"7",
"6500k"->"3",
"native"->"4"
|>

wbPossible = {"auto","3000k","4000k","4800k","5500k","6000k","6500k","native"}
goProGetPossibleWhiteBalance[] :=
    wbPossible
goProSetPhotoWhiteBalance[param_String] :=
    execute[goProMakeCommand["setting",wbPhoto,wbToCode[[ToLowerCase[param] ]]]]

goProSetVideoWhiteBalance[param_String] :=
    execute[goProMakeCommand["setting",wbVideo,wbToCode[[ToLowerCase[param] ]]]]	
    

goProSetMultiShotWhiteBalance[param_String] :=
    execute[goProMakeCommand["setting",wbMulti,wbToCode[[ToLowerCase[param] ]]]]	
    
    

(* ::Subsection:: *)
(* Collor Profile *)
(*Color Profile *)

coVideo="12"
coPhoto="23"
coMulti="36"
coToCode = <|
    "gopro"->"0",
    "flat"->"1"
|>;
coPossible = {"GoPro","Flat"};
goProGetPossibleColorProfile[] :=coPossible
goProSetVideoColorProfile[param_String] := execute[goProMakeCommand["setting",coVideo,coToCode[[ToLowerCase[param] ]]]]
goProSetPhotoColorProfile[param_String] := execute[goProMakeCommand["setting",coPhoto,coToCode[[ToLowerCase[param] ]]]]
goProSetMultiShotColorProfile[param_String] :=execute[goProMakeCommand["setting",coMulti,coToCode[[ToLowerCase[param] ]]]]	


(* ::Subsection:: *)
(* ISO *)
(*ISO*)
isoLimitVideoToCode = <|
    "6400"->"0",
"1600"->"1",
"400"->"2",
"3200"->"3",
"800"->"4",
"200"->"7",
"100"->"8"
|>;
isoLimitVideoPossible = {"6400","1600","400","3200","800","200","100"};
isoLimitPhotoToCode = <|
"800"->"0",
"400"->"1",
"200"->"2",
"100"->"3"
|>;
isoLimitPhotoPossible = {"800","400","200","100"};
isoLimitMultiToCode = <|
"800"->"0",
"400"->"1",
"200"->"2",
"100"->"3"
|>;
isoLimitMultiPossible = {"800","400","200","100"};

isoLimitVideo="13"
isoLimitPhoto="24"
isoLimitMulti="37"


goProGetPossibleVideoIsoLimit[]:=isoLimitVideoPossible
goProGetPossiblePhotoIsoLimit[]:=isoLimitPhotoPossible
goProGetPossibleMultiShotIsoLimit[]:=isoLimitMultiPossible

goProSetVideoIsoLimit[param_String]:=execute[goProMakeCommand["setting",isoLimitVideo,isoLimitVideoToCode[[ToLowerCase[param] ]]]]
goProSetPhotoIsoLimit[param_String]:=execute[goProMakeCommand["setting",isoLimitPhoto,isoLimitPhotoToCode[[ToLowerCase[param] ]]]]
goProSetMultiShotIsoLimit[param_String]:=execute[goProMakeCommand["setting",isoLimitMulti,isoLimitMultiToCode[[ToLowerCase[param] ]]]]


(* ::Subsection:: *)
(* ISO MODE *)
isoMode="74"
isoModePossible={"max","lock"}
isoModeToCode=<|"max"->"0","lock"->"1"|>

goProSetIsoMode[param_String]:=execute[goProMakeCommand["setting",isoMode,isoModeToCode[[ToLowerCase[param] ]]]]
goProGetPossibleIsoMode[]:=isoModePossible



(* ::Subsection:: *)
(* ISO MIN *)
isoMinPossible = {"800","400","200","100"};
isoMinToCode = <|
"800"->"0",
"400"->"1",
"200"->"2",
"100"->"3"
|>;

isoMinPhoto="75"
isoMinMulti="76"

goProSetPhotoIsoMinimum[param_String]:=execute[goProMakeCommand["setting",isoMinPhoto,isoMinToCode[[ToLowerCase[param] ]]]]
goProGetPossiblePhotoIsoMinimum[]:=isoMinPossible

goProSetMultiShotIsoMinimum[param_String]:=execute[goProMakeCommand["setting",isoMinMulti,isoMinToCode[[ToLowerCase[param] ]]]]
goProGetPossibleMultiShotIsoMinimum[]:=isoMinPossible


(* ::Subsection:: *)
(* Sharpness *)
sharpnessPossible={"high","medium","low"};

sharpnessToCode=<|"high"->"0","medium"->"1","low"->"2"|>

sharpnessVideo="14"
sharpnessPhoto="25"
sharpnessMultiShot="38"

goProGetPossibleSharpness[]:=sharpnessPossible

goProSetVideoSharpnes[param_String]:=execute[goProMakeCommand["setting",sharpnessVideo,sharpnessToCode[[ToLowerCase[param] ]]]]
goProSetPhotoSharpnes[param_String]:=execute[goProMakeCommand["setting",sharpnessPhoto,sharpnessToCode[[ToLowerCase[param] ]]]]
goProSetMultiShotSharpnes[param_String]:=execute[goProMakeCommand["setting",sharpnessMultiShot,sharpnessToCode[[ToLowerCase[param] ]]]]



(* ::Subsection:: *)
(* VideoResolution *)
videoResToCodeBlack=<| 
"4K"->"1",
"4K SuperView"->"2",
"2.7K"->"4",
"2.7K SuperView"->"5",
"2.7K 4:3"->"6",
"1440p"->"7",
"1080p SuperView"->"8",
"1080p"->"9",
"960p"->"10",
"720p SuperView"->"11",
"720p"->"12",
"WVGA"->"13"
|>
videoResToCodeSilver=<| 
"4K"->"1",
"2.7K"->"4",
"2.7K 4:3"->"6",
"1440p"->"7",
"1080p SuperView"->"8",
"1080p"->"9",
"960p"->"10",
"720p SuperView"->"11",
"720p"->"12",
"WVGA"->"13"
|>
videoResToCodeSession=<|
"1440p"->"7",
"1080p SuperView"->"8",
"1080p"->"9",
"960p"->"10",
"720p SuperView"->"11",
"720p"->"12",
"WVGA"->"13"
|>
videoRes="2"

videoResPossibleBlack=Keys[videoResToCodeBlack];
videoResPossibleSilver=Keys[videoResToCodeSilver];
videoResPossibleSession=Keys[videoResToCodeSession];

goProSetCameraModel::model="First you have to specificate camera model! Call goProSetCameraModel[_String]";

goProGetPossibleVideoResolution[]:=Switch[model,"Black",videoResPossibleBlack,
	"Silver",videoResPossibleSilver,
	"Session",videoResPossibleSession,
	_, Message[goProSetCameraModel::model, model]
	]
goProSetVideoResolution[param_String]:=Switch[model,"Black",execute[goProMakeCommand["setting",videoRes,videoResToCodeBlack[[param]]]],
	"Silver",execute[goProMakeCommand["setting",videoRes,videoResToCodeSilver[[param]]]],
	"Session",execute[goProMakeCommand["setting",videoRes,videoResToCodeSession[[param]]]],
	_, Message[goProSetCameraModel::model, model]
	]
		
	
(* ::Subsection:: *)
(* FPS *)	
fpsToCode=<|
"240"->"0",
"120"->"1",
"100"->"2",
"90"->"3",
"80"->"4",
"60"->"5",
"50"->"6",
"48"->"7",
"30"->"8",
"25"->"9"
|>
fpsPossible={"240","120","100","90","80","60","50","48","30","25"}
fps="3";
(*toDo*)
fpsPossibleSilver={}
fpsToCodeSilver=<||>


goProGetPossibleFPS[]:=Switch[model,"Black",fpsPossible,
	"Silver",fpsPossibleSilver,
	"Session",fpsPossible,
	_, Message[goProSetCameraModel::model, model]
	]

	
goProSetFPS[param_String]:=Switch[model,"Black",execute[goProMakeCommand["setting",fps,fpsToCode[[param]]]],
	"Silver",execute[goProMakeCommand["setting",fps,fpsToCodeSilver[[param]]]],
	"Session",execute[goProMakeCommand["setting",fps,fpsToCode[[param]]]],
	_, Message[goProSetCameraModel::model, model]
	]


(* ::Subsection:: *)
(* FOV *)
fov="4"
fovToCode=<|
	"wide"->"0",
	"medium"->"1",
	"narrow"->"2",
	"linear"->"4"
|>
fovPossible={"wide","medium","narrow","linear"}
goProGetPossibleFOV[]:=fovPossible
goProSetFOV[param_String]:=execute[goProMakeCommand["setting",fov,fovToCode[[param]]]]



(* ::Subsection:: *)
(* LowLight *)
lowLight="8"
lowLightToCode=<|"on"->"1","off"->"0"|>
lowLightPossible={"on","off"}
goProSetLowLight[param_String]:=execute[goProMakeCommand["setting",lowLight,lowLightToCode[[param]]]]
goProGetPossibleLowLight[]:=lowLightPossible
goProSwitchLowLightOn[]:=goProSetLowLight["on"]
goProSwitchLowLightOff[]:=goProSetLowLight["off"]


(* ::Subsection:: *)
(* Spot Meter *)
spotMeter="9"
spotMeterToCode=<|"on"->"1","off"->"0"|>
spotMeterPossible={"on","off"}
goProSetSpotMeter[param_String]:=execute[goProMakeCommand["setting",spotMeter,spotMeterToCode[[param]]]]
goProGetPossibleSpotMeter[]:=spotMeterPossible
goProSwitchSpotMeterOn[]:=goProSetSpotMeter["on"]
goProSwitchSpotMeterOff[]:=goProSetSpotMeter["off"]




	
(* ::Subsection:: *)
(* VIDEO Shutter Speed *)
shutterSpeedToCode=<|
	
|>
shutterSpeedPossible={}

(*goProPossibleVideoShutterSpeed[]:=*)




(* ::End:: *)
(* Region Title *)

End[]

EndPackage[]