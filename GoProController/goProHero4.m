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

goProGetPossibleVideoSpotMeter::usage =
    "goProGetPossibleVideoSpotMeter[ ] returns possible parameters for SpotMeter settings"
goProSetVideoSpotMeter::usage =
    "goProSetVideoSpotMeter[ _String] sets SpotMeter to parameter given. To see which parameters are supported call goProGetPossibleVideoSpotMeter[]."
goProSwitchVideoSpotMeterOn::usage =
    "goProSwitchVideoSpotMeterOn[ ] turns SpotMeter on."
goProSwitchVideoSpotMeterOff::usage =
    "goProSwitchVideoSpotMeterOff[ ] turns SpotMeter off."


(* ::Subsection:: *)
(* Video Loop *)
goProGetPossibleVideoLoop::usage =
    "goProGetPossibleVideoLoop[ ] returns possible values of Video Loop"
goProSetVideoLoop::usage =
    "goProSetVideoLoop[_String] Sets Video Loop to parameter given.
	 To see which parameters you can use for Video Loop use function goProGetPossibleVideoLoop[ ]"



(* ::Subsection:: *)
(* Photo In video *)
goProGetPossiblePhotoInVideo::usage =
    "goProGetPossiblePhotoInVideo[ ] returns possible vaules of time in which will camera take photo during video recording."
goProSetPhotoInVideo::usage =
    "goProSetPhotoInVideo[ _String] Sets time interval in which will camera take photo during video recording."  




(* ::Subsection:: *)
(* Photo Resolution *)

goProSetPhotoRes::usage =
    "goProSetPhotoRes[_String] Sets Video resolution acording to parameter. To see which parameter you can be used use function goProGetPossiblePhotoRes[]"
goProGetPossiblePhotoRes::usage =
    "goProGetPossiblePhotoRes[ ] Returns list of possible parameters for function goProSetVideoRes[_String]"




(* ::Subsection:: *)
(* NightPhoto time exposure *)
goProGetPossibleNightPhotoExposureTime::usage=
	"goProGetPossibleNightPhotoExposureTime[] returns possible values for Night Photo Exposure time."
goProSetNightPhotoExposureTime::usage=
	"goProSetNightPhotoExposureTime[ _String] sets Exposure Time for Night Photo to parameter given, 
	to see which parameter you can use call goProGetPossibleNightPhotoExposureTime[] function."



(* ::Subsection:: *)
(* Spot Meter *)

goProGetPossiblePhotoSpotMeter::usage =
    "goProGetPossiblePhotoSpotMeter[ ] returns possible parameters for SpotMeter settings"
goProSetPhotoSpotMeter::usage =
    "goProSetPhotoSpotMeter[ _String] sets SpotMeter to parameter given. To see which parameters are supported call goProGetPossiblePhotoSpotMeter[]."
goProSwitchPhotoSpotMeterOn::usage =
    "goProSwitchPhotoSpotMeterOn[ ] turns SpotMeter on."
goProSwitchPhotoSpotMeterOff::usage =
    "goProSwitchPhotoSpotMeterOff[ ] turns SpotMeter off."



(* ::Subsection:: *)
(* Continuous Shot *)    
goProGetPossibleContinuousShot::usage =
    "goProGetContinuousShot[ ] returns possible values of Continuous Shot."
goProSetContinuousShot::usage =
    "goProSetContinuousShot[ _String] Sets Continuous Shot to parameter given."


(* ::Subsection:: *)
(* Exposure Time for NightLapse *)
goProGetPossibleNightLapseExposureTime::usage=
	"goProGetPossibleNightLapseExposureTime[] returns possible values for NightLapse Exposure time."
goProSetNightLapseExposureTime::usage=
	"goProSetNightLapseExposureTime[ _String] sets NightLapse exposure time to value given.
	to see which values you can use call goProGetPossibleNightLapseExposureTime[]. "



(* ::Subsection:: *)
(* Night Lapse interval *)
goProGetPossibleNightLapseInterval::usage=
	"goProGetPossibleNightLapseInterval[] returns value usable for night lapse time interval."
goProSetNightLapseInterval::usage=
	"goProSetNightLapseInterval[ _String] sets time interval of night lapse to parameter given."




(* ::Subsection:: *)
(* MultiShot resolution *)
goProSetMultiShotResolution::usage=
	"goProSetMultiShotResolution[ _String] sets resolution of multi shot to parameter given."
goProGetPossibleMultiShotResolution::usage=
	"goProGetPossibleMultiShotResolution[ ] returns values usable for multi shot resolution."



(* ::Subsection:: *)
(* TimeLapse interval *)
goProSetTimeLapseInterval::usage=
	"goProSetTimeLapseInterval[ _String] set TimeLapse interval to parameter given."
goProGetPossibleTimeLapseInterval::usage=
	"goProGetPossibleTimeLapseInterval[ ] returns usable values for TimeLapse interval."
	
	
	

(* ::Subsection:: *)
(* MultiShot spot meter *)
goProGetPossibleMultiShotSpotMeter::usage =
    "goProGetPossibleMultiShotSpotMeter[ ] returns possible parameters for SpotMeter settings"
goProSetMultiShotSpotMeter::usage =
    "goProSetMultiShotSpotMeter[ _String] sets SpotMeter to parameter given. To see which parameters are supported call goProGetPossibleMultiShotSpotMeter[]."
goProSwitchMultiShotSpotMeterOn::usage =
    "goProSwitchMultiShotSpotMeterOn[ ] turns SpotMeter on."
goProSwitchMultiShotSpotMeterOff::usage =
    "goProSwitchMultiShotSpotMeterOff[ ] turns SpotMeter off."
	
	


(* ::Subsection:: *)
(* BurstRate *)
goProGetPossibleBurstRate::usage =
    "goProGetPossibleBurstRate[ ] returns possible values of BurstRate"
goProSetBurstRate::usage =
    "goProSetBurstRate[_String] Sets BurstRate to parameter given.
	 To see which parameters you can use for BurstRate use function goProGetPossibleBurstRate[ ]"
	
	



(* ::Subsection:: *)
(* Orientation *)
(*Orientation*)
goProSetOrientation::usage =
    "goProSetOrientation[ _String] sets orientation of your display based on parameter."
goProGetPossibleOrientation::usage =
    "goPoGetPossibleOrientation[ ] returns possible parameters for orientation of display."
    
    

(* ::Subsection:: *)
(* QuickCature *)
goProGetPossibleQuickCapture::usage=
	"goProGetPossibleQuickCapture[ ] returns values usable for quick capture."
goProSetQuickCapture::usage=
	"goProSetQuickCapture[ _String] sets quick capture to parameter given."
goProSwitchQuickCaptureOn::usage=
	"goProSwitchQuickCaptureOn[ ] switches quick capture on."
goProSwitchQuickCaptureOff::usage=
	"goProSwitchQuickCaptureOff[ ] switches quick capture off."



(* ::Subsection:: *)
(* LED *)
goProGetPossibleLed::usage =
    "goProGetPossibleLed[ ] returns possible parameters for Led settings"
goProSetLed::usage =
    "goProSetLed[ _String] sets leds to parameter given. To see which parameter you can use call goProGetPossibleLed[]."

(* ::Subsection:: *)
(* Volume *)
goProGetPossibleVolume::usage =
    "goProGetPossibleVolume[ ] returns possible values for volume settings."
goProSetVolume::usage =
    "goProSetVolume[ _String] sets volume to parameter given. For values which are usable see goProGetPossibleVolume[]."
 


(* ::Subsection:: *)
(* Video Mode *)
goProGetPossibleVideoMode::usage =
    "goProGetPossibleVideoMode[ ] returns possible parameters for VideoMode settings"
goProSetVideoMode::usage =
    "goProSetVideoMode[ _String] sets leds to parameter given. To see which parameter you can use call goProGetPossibleVideoMode[]."




(* ::Subsection:: *)
(* LCD Display *)
goProSwitchLCDDisplayOn::usage=
	"goProSetLCDDisplayOn[ ] switches LCD display on" 
goProSwitchLCDDisplayOff::usage=
	"goProSetLCDDisplayOff[ ] switches LCD display off"



(* ::Subsection:: *)
(* AutoPowerOff *)
(*Auto Power Off*)
goProGetPossibleAutoPowerOff::usage =
    "goProGetPossibleAutoPowerOff[ ] returns possible parameters in minutes for AutoPowerOff settings"
goProSetAutoPowerOff::usage =
    "goProSetAutoPowerOff[ _String] sets AutoPowerOff to parameter given. To see which parameters are supported call goProGetPossibleAutoPowerOff []."




(* ::Subsection:: *)
(* Locate *)
goProLocate::usage =
    "goProLocate[ ] camera will start sound signal to locate your camera."
goProLocateStop::usage =
    "goProLocateStop[ ] camera will stop sound signal to locate your camera."



(* ::Subsection:: *)
(* Delete *)
goProDeleteFile::usage=
	"goProDeleteFile[ _String] deletes file which name was given as parameter. Insert name in format /100GOPRO/file"
goProDeleteLast::usage=
	"goProDeleteLast[ ] deletes last file from memory."
goProDeleteAll::usage=
	"goProDeleteAll[ ] deletes all files from memory."






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
videoSpotMeter="9"
spotMeterToCode=<|"on"->"1","off"->"0"|>
spotMeterPossible={"on","off"}
goProSetVideoSpotMeter[param_String]:=execute[goProMakeCommand["setting",videoSpotMeter,spotMeterToCode[[param]]]]
goProGetPossibleVideoSpotMeter[]:=spotMeterPossible
goProSwitchVideoSpotMeterOn[]:=goProSetVideoSpotMeter["on"]
goProSwitchVideoSpotMeterOff[]:=goProSetVideoSpotMeter["off"]





(* ::Subsection:: *)
(* Video Loop *)
videoLoop="6"
videoLoopToCode = <|
   "5"->"1",
   "20"->"2",
   "60"->"3",
   "120"->"4",
   "max"->"0"
   |>;
videoLoopPossible = {"max","5","20","60","120"}
goProGetPossibleVideoLoop[] :=videoLoopPossible
goProSetVideoLoop[param_String]:=execute[goProMakeCommand["setting",videoLoop,videoLoopToCode[[param]]]]



(* ::Subsection:: *)
(* PhotonVideo *)
photoInVideo="7"
photoInVideoToCode = <|
    "5"->"1",
    "10"->"2",
    "30"->"3",
    "60"->"4"
|>

photoInVideoPossible = {"5","10","30","60"}
goProGetPossiblePhotoInVideo[] :=photoInVideoPossible
goProSetPhotoInVideo[param_String] :=execute[goProMakeCommand["setting",photoInVideo,photoInVideoToCode[[param]]]]





(* ::Subsection:: *)
(* Photo Resolution *)

photoRes="17"
photoResToCode = <|
   "5M" -> "3",
   "7W" -> "1",
   "12W" -> "0",
   "7M" -> "2"
   |>;
     
photoResPossible = {
   "12W",
   "7W",
   "7M",
   "5M"
};

 
goProSetPhotoRes[param_String] := execute[goProMakeCommand["setting",photoRes,photoResToCode[[param]]]]
goProGetPossiblePhotoRes[] :=photoResPossible



(* ::Subsection:: *)
(* NightPhotoExposureTime *)
nightPhotoExTime="19"
nightPhotoExTimeToCode=<|"auto"->"0",
"2"->"1",
"5"->"2",
"10"->"3",
"15"->"4",
"20"->"5",
"30"->"6"
|>
nightPhotoExTimePossible={"auto","2","5","10","15","20","30"}
goProGetPossibleNightPhotoExposureTime[]:=nightPhotoExTimePossible
goProSetNightPhotoExposureTime[param_String]:=execute[goProMakeCommand["setting",nightPhotoExTime,nightPhotoExTimeToCode[[param]]]]


(* ::Subsection:: *)
(* Photo Spot Meter *)
photoSpotMeter="20"
goProSetPhotoSpotMeter[param_String]:=execute[goProMakeCommand["setting",photoSpotMeter,spotMeterToCode[[param]]]]
goProGetPossiblePhotoSpotMeter[]:=spotMeterPossible
goProSwitchPhotoSpotMeterOn[]:=goProSetPhotoSpotMeter["on"]
goProSwitchPhotoSpotMeterOff[]:=goProSetPhotoSpotMeter["off"]




(* ::Subsection:: *)
(* ContinuousPhoto *)
continuousShot="18"
continuousShotToCode = <|
   "3"->"0",
   "5"->"1",
   "10"->"2"
   |>;
continuousShotPossible = {"3","5","10"}
goProGetPossibleContinuousShot[] :=continuousShotPossible
goProSetContinuousShot[param_String] :=execute[goProMakeCommand["setting",continuousShot,continuousShotToCode[[param]]]]




(* ::Subsection:: *)
(* NightLapseExposureTime *)

nightLapseExTime="31"
nightLapseExTimeToCode=<|"auto"->"0",
"2"->"1",
"5"->"2",
"10"->"3",
"15"->"4",
"20"->"5",
"30"->"6"
|>
nightLapseExTimePossible={"auto","2","5","10","15","20","30"}
goProGetPossibleNightLapseExposureTime[]:=nightLapseExTimePossible
goProSetNightLapseExposureTime[param_String]:=execute[goProMakeCommand["setting",nightLapseExTime,nightLapseExTimeToCode[[param]]]]

	
	

(* ::Subsection:: *)
(* NightLapseTimeInterval *)
nightLapseInterval="32"
nightLapseIntervalPossible={"continuous","4s","5s","10s","15s","20s","30s","1m","2m","5m","30m","60m"}
nightLapseIntervalToCode=<|
"continuous"->"0",
"4s"->"4",
"5s"->"5",
"10s"->"10",
"15s"->"15",
"20s"->"20",
"30s"->"30",
"1m"->"60",
"2m"->"120",
"5m"->"300",
"30m"->"1800",
"60m"->"3600"
|>
goProGetPossibleNightLapseInterval[]:=nightLapseIntervalPossible
goProSetNightLapseInterval[param_String]:=execute[goProMakeCommand["setting",nightLapseInterval,nightLapseIntervalToCode[[param]]]]
	
	
	
	

(* ::Subsection:: *)
(* MultiShotresolution *)
multiShotResolution="28"
goProSetMultiShotResolution[param_String]:=execute[goProMakeCommand["setting",multiShotResolution,photoResToCode[[param]]]]
goProGetPossibleMultiShotResolution[]:=photoResPossible
	


(* ::Subsection:: *)
(* TimeLapse Interval *)
timeLapseInterval="30"
timeLapseIntervalToCode=<|"0.5"->"0",
"1"->"1",
"2"->"2",
"5"->"5",
"10"->"10",
"30"->"30",
"60"->"60"
|>
timeLapseIntervalPossible={"0.5","1","2","5","10","30","60"}
goProSetTimeLapseInterval[param_String]:=execute[goProMakeCommand["setting",timeLapseInterval,timeLapseIntervalToCode[[param]]]]
goProGetPossibleTimeLapseInterval[]:=timeLapseIntervalPossible

(* ::Subsection:: *)
(* Photo Spot Meter *)
multiShotSpotMeter="33"
goProSetMultiShotSpotMeter[param_String]:=execute[goProMakeCommand["setting",multiShotSpotMeter,spotMeterToCode[[param]]]]
goProGetPossibleMultiShotSpotMeter[]:=spotMeterPossible
goProSwitchMultiShotSpotMeterOn[]:=goProSetMultiShotSpotMeter["on"]
goProSwitchMultiShotSpotMeterOff[]:=goProSetMultiShotSpotMeter["off"]




(* ::Subsection:: *)
(* Burst Rate *)
burstRate="29"
burstRateToCode = <|
   "3/1"->"0",
   "5/1"->"1",
   "10/1"->"2",
   "10/2"->"3",
   "10/3"->"4",
   "30/1"->"5",
   "30/2"->"6",
   "30/3"->"7",
   "30/6"->"8"
   |>;
burstRatePossible = {"3/1","5/1","10/1","10/2","10/3","30/1","30/2","30/3","30/6"}

goProGetPossibleBurstRate[] := burstRatePossible
goProSetBurstRate[param_String] := execute[goProMakeCommand["setting",burstRate,burstRateToCode[[param]]]]




(* ::Subsection:: *)
(* Orientation *)
orientation="52"
orientationPossible = {"up","down","gyro"}
orientationToCode=<|"up"->"1","down"->"2","gyro"->"0"|>
goProSetOrientation[param_String]:=execute[goProMakeCommand["setting",orientation,orientationToCode[[param]]]]
goProGetPossibleOrientation[]:=orientationPossible



(* ::Subsection:: *)
(* Quick Capture *)
quickCapture="54"
goProGetPossibleQuickCapture[]:=spotMeterPossible
goProSetQuickCapture[param_String]:=execute[goProMakeCommand["setting",quickCapture,spotMeterToCode[[param]]]]
goProSwitchQuickCaptureOn[]:=goProSetQuickCapture["on"]
goProSwitchQuickCaptureOff[]:=goProSetQuickCapture["off"]



(* ::Subsection:: *)
(* LED *)
led="55"
ledToCode = <|
    "off"->"0",
    "2"->"1",
    "4"->"2"
|>
ledPossible = {"off","2","4"}
goProGetPossibleLed[] := ledPossible
goProSetLed[param_String] := execute[goProMakeCommand["setting",led,ledToCode[[param]]]]



(* ::Subsection:: *)
(* Volume *)
volume="56"
volumeToCode = <|
    "off"->"2",
    "70"->"1",
    "100"->"0"
|>
volumePossible = {"off","70","100"}
goProGetPossibleVolume[] :=volumePossible
goProSetVolume[param_String] := execute[goProMakeCommand["setting",volume,volumeToCode[[param]]]]
 




(* ::Subsection:: *)
(* VideoMode *)
videoMode="57"
videoModeToCode = <|
    "ntsc"->"0",
    "pal"->"1"
|>
videoModePossible = {"ntsc","pal"}
goProGetPossibleVideoMode[] := videoModePossible
goProSetVideoMode[param_String] :=execute[goProMakeCommand["setting",videoMode,videoModeToCode[[param]]]]


(* ::Subsection:: *)
(* LCD Display *)
lcdDisplay="72"
goProSwitchLCDDisplayOn[]:=execute[goProMakeCommand["setting",lcdDisplay,"1"]]
goProSwitchLCDDisplayOff[]:=execute[goProMakeCommand["setting",lcdDisplay,"0"]]



(* ::Subsection:: *)
(* LCD display Brightness *)
(*lcdBrightness="49"
lcdBrightnessToCode=<|"high"->"0","medium"->"1","low"->"2"|>
lcdBrightnessPossible={"high","medium","low"}
goProSetLCDBrightness[param_String]:=execute[goProMakeCommand["setting",lcdBrightness,lcdBrightnessToCode[[param]]]]
goProGetPossibleLCDBrightness[]:=lcdBrightnessPossible*)



(* ::Subsection:: *)
(* Auto off	 *)
autoOff="59"
autoOffToCode=<|"never"->"0",
"1"->"1",
"2"->"2",
"3"->"3",
"5"->"4"|>
autoOffPossible={"never","1","2","3","5"}
goProSetAutoPowerOff[param_String]:=execute[goProMakeCommand["setting",autoOff,autoOffToCode[[param]]]]
goProGetPossibleAutoPowerOff[]:=autoOffPossible



(* ::Subsection:: *)
(* Locate *)

goProLocate[] :=execute[goProMakeCommand["command/system","locate?p=1"]]
goProLocateStop[] :=execute[goProMakeCommand["command/system","locate?p=0"]]



(* ::Subsection:: *)
(* Delete *)
goProDeleteFile[param_String]:=execute[goProMakeCommand["command/storage","delete?p="<>param]]
goProDeleteLast[]:=execute[goProMakeCommand["command/storage/delete","last"]]
goProDeleteAll[]:=execute[goProMakeCommand["command/storage/delete","all"]]




	
	
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