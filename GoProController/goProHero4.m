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

goProSetVideoSharpness::usage=
	"goProSetVideoSharpnes[ _String] sets Video sharpness to parameter given, to see which parameter you can use call goProGetPossibleSharpness function."
	
goProSetPhotoSharpness::usage=
	"goProSetPhotoSharpness[ _String] sets Photo sharpness to parameter given, to see which parameter you can use call goProGetPossibleSharpness function."
	
goProSetMultiShotSharpness::usage=
	"goProSetMultiShotSharpness[ _String] sets MultiShot sharpness to parameter given, to see which parameter you can use call goProGetPossibleSharpness function."





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
goProGetPossibleFPS::usage =
    "goProGetPossibleFPS[ _String] Returns list of possible FPS for video resolution specified by parameter"
goProGetCurrentlyPossibleFPS::usage=
	"goProGetCurrentlyPossibleFPS[] Returns possible FPS for current video resolution."   
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
goProSwitchSpotMeterOn::usage =
    "goProSwitchPhotoSpotMeterOn[ ] turns SpotMeter on."
goProSwitchSpotMeterOff::usage =
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
goProSetMultiShotTimeLapseInterval::usage=
	"goProSetMultiShotTimeLapseInterval[ _String] set TimeLapse for MultiShot interval to parameter given."
goProSetVideoTimeLapseInterval::usage=
	"goProSetVideoTimeLapseInterval[ _String] set TimeLapse for Video interval to parameter given."
goProGetPossibleTimeLapseInterval::usage=
	"goProGetPossibleTimeLapseInterval[ ] returns usable values for TimeLapse interval."
goProSetTimeLapseInterval::usage=
	"goProSetVideoTimeLapseInterval[ _String] set TimeLapse for Video interval to parameter given."	
	
	


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
(*goProDeleteFile::usage=
	"goProDeleteFile[ _String] deletes file which name was given as parameter. Insert name in format /100GOPRO/file"*)
goProDeleteLast::usage=
	"goProDeleteLast[ ] deletes last file from memory."
goProDeleteAll::usage=
	"goProDeleteAll[ ] deletes all files from memory."




(* ::Subsection:: *)
(* Exposure *)
goProGetPossibleExposure::usage =
    "goProGetPossibleExposure[ ] returns possible parameters for exposure compensation settings"
goProSetExposure::usage =
    "goProSetExposure[ _String] sets exposure compensation to parameter given. The range is (-2 to +2), in 0.5 step increments. To see which parameters are supported call goProGetPossibleExposure[]. Only if protune mod is on."
goProSetPhotoExposure::usage =
    "goProSetExposure[ _String] sets exposure compensation to parameter given. The range is (-2 to +2), in 0.5 step increments. To see which parameters are supported call goProGetPossibleExposure[]. Only if protune mod is on."
goProSetVideoExposure::usage =
    "goProSetExposure[ _String] sets exposure compensation to parameter given. The range is (-2 to +2), in 0.5 step increments. To see which parameters are supported call goProGetPossibleExposure[]. Only if protune mod is on."
goProSetMultiShotExposure::usage =
    "goProSetExposure[ _String] sets exposure compensation to parameter given. The range is (-2 to +2), in 0.5 step increments. To see which parameters are supported call goProGetPossibleExposure[]. Only if protune mod is on."




(* ::Subsection:: *)
(* Shutter Time *)
goProGetPossibleShutterTime::usage=
	"goProGetPossibleShutterTime[ ] returns possible values for shutterTime."
goProGetPossibleShutterTime::usage=
	"goProGetPossibleShutterTime[ _String] returns possible values for shutterTime for FPS given as parameter."
goProGetCurrentlyPossibleShutterTimeFPS::usage=
	"goProGetCurrentlyPossibleShutterTimeFPS[ ] returns possible values for shutterTime for current FPS set."
goProSetShutterTime::usage=
	"goProSetShutterTime[_String] Sets time when shutter is open to parameter given (Only in Video Mode)."




(* ::Subsection:: *)
(* Protune *)
goProGetPossibleProtune::usage =
    "goProGetPossibleProtune[ ] returns possible values for protune mode."
goProSetProtune::usage =
    "goProSetProtune[ _String] switch protune mode on/off."

goProSetVideoProtune::usage =
    "goProSetVideoProtune[ _String] switch protune mode on/off."
goProSetPhotoProtune::usage =
    "goProSetPhotoProtune[ _String] switch protune mode on/off."
goProSetMultiShotProtune::usage =
    "goProSetMultiShotProtune[ _String] switch protune mode on/off."   
 
goProSwitchProtuneOn::usage =
    "goProSwitchProtuneOn[ ] Switch protune mode on."
goProSwitchProtuneOff::usage =
    "goProSwitchProtuneOff[ ] Switch protune mode off."




(* ::Subsection:: *)
(* CameraStatus *)
goProGetCameraStatus::usage=
	"goProGetCameraStatus[ ] Prints status of camera."



(* ::Subsection:: *)
(* Camera Settings *)
goProGetSettingReport::usage =
    "goProGetSettingReport[ ] returns settings of camera"
goProGetVideoSettingReport::usage =
    "goProGetVideoSettingReport[ ] returns Video settings of camera"    
goProGetPhotoSettingReport::usage =
    "goProGetPhotoSettingReport[ ] returns Photo settings of camera"    
goProGetMultiShotSettingReport::usage =
    "goProGetMultiShotSettingReport[ ] returns MultiShot settings of camera"    

    
    

(* ::Subsection:: *)
(* Download *)
goProGetFileList::usage=
	"goProGetFileList[] returns list of files which were aquired by the camera." 
goProDownloadFile::usage=
	"goProDownloadFile[name_String,destination_String] download file which name was given as first parameter to the destination specified in second parameter. "
goProDownloadAllFiles::usage=
	"goProDownloadAll[ _String] downloads all files from camera to destination given."
goProGetFileURL::usage=
	"goProGetFileURL[_String] returns URL string of file which name was given in parameter."	



(* ::Subsection:: *)
(* Modes *)
goProSetBootMode::usage=
	"goProSetBootMode[ _String] Sets boot mode to parameter given, to see options possible call goProGetPossibleBootMode[]. "
goProGetPossibleBootMode::usage=
	"goProGetPossibleBootMode[] returns possible options for boot mode.  "	
	
goProMode::usage=
	"goProMode[ _String] Switches mode to parameter given. To see which parameter you can use call goProGetPossibleModes[]."
goProGetPossibleModes::usage=
	"goProGetPossibleModes[ ] returns possible camera modes ."
	
goProSetURLBase::usage=
	"goProSetURLBase[_String] lets you set urlBase variable. Use it if you want to set another folder on camera for downloading data from camera."

goProGetURLBase::usage=
	"goProGetURLBase[] returns value of urlBase variable, which stores url for folder from which you can download data from camera."
	
	


(* ::Subsection:: *)
(* Power *)
(*goProTurnOn::usage = 
    "goProTurnOn[ ] Turns on the gopro camera"*)

goProTurnOff::usage =    
    "goProTurnOn[ ] Turns off the gopro camera"
    
    
    

(* ::Subsection:: *)
(* complete Setup *)
goProSet::usage =
    "goProSet[ ] enables to set camera via rules such as: {fps,fov,videoResolution,photoResolution,...}"
   
goProGetSettingReportAssociation::usage=
	"goProGetSettingReportAssociation[ ] returns association field with all settings of camera."
	
goProGetVariables::usage=
	"goProGetVariables[ ], returns all variables for goProSet function."
	
goProGet::usage=
	"goProGet[_String] returns value set to parameter given such ase {videoResolution->1080p}"
	
	
(* ::Section:: *)
(* Private Definitions *)	

Begin["`Private`"] (* Begin Private Context *) 

camera = "HERO4";
model="";

goProGetCamera[] := camera

modelPossible={"Black","Silver","Session"}
goProGetPossibleCameraModel[]:=modelPossible

goProSetCameraModel::wrong="You can't set model to `1`, please see goProGetPossibleCameraModel to get list of usable parameters."
goProSetCameraModel[param_String]:=If[MemberQ[modelPossible,param],model=param,Message[goProSetCameraModel::wrong,param]]
goProGetCameraModel[]:=model;



goProUrl = "http://10.5.5.9/gp/gpControl/";

(*function for putting together url for GoProCommand,
 mode={setting,command}
*)
goProMakeCommand[mode_String,param1_String,param2_String]:= goProUrl <> mode <> "/" <> param1 <> "/" <> param2;

goProMakeCommand[mode_String,param_String] := goProUrl <> mode <> "/" <> param;

(*spusteni prikazu exec pomoci HTTPRequest a URLRead*)
execute[exec_String] :=(request = HTTPRequest[exec];
     URLRead[request])
 
 
 
 
 
 

(* ::Subsection:: *)
(* Power *)
(*
macAddress=""
goProSetMac[param_String]:=macAdress=param


goProSetMac::mac="You have to set mac adress of camera first, use goProSetMac[_String]";
*)



goProTurnOff[]:=execute[goProMakeCommand["command","system","sleep"]]
    
 
 
 

(* ::Subsection:: *)
(* Modes *)

bootModeCode="53"
bootModeToCode=<|
	"Video"->"0",
	"Photo"->"1",
	"MultiShot"->"2"
|>

goProSetBootMode[param_String]:=execute[goProMakeCommand["setting",bootModeCode,bootModeToCode[[param]] ]]
goProGetPossibleBootMode[]:=Keys[bootModeToCode]
	
	
modesToCode=<|
	"Video"->{"0","0"},
	"TimeLapseVideo"->{"0","1"},
	"PhotoInVideo"->{"0","2"},
	"LoopingVideo"->{"0","3"},
	"Photo"->{"1","0"},
	"ContinuousPhoto"->{"1","1"},
	"NightPhoto"->{"1","2"},
	"Burst"->{"2","0"},
	"TimeLapse"->{"2","1"},
	"NightLapse"->{"2","2"}
|>	

goProGetPossibleModes[]:=Keys[modesToCode]
goProMode[param_String]:=execute[goProMakeCommand["command","sub_mode?mode="<>modesToCode[[param]][[1]] <>
	"&sub_mode=" <> modesToCode[[param]][[2]] ]]

 
 
(* ::Subsection:: *)
(* Protune *)
videoProtuneCode="10"
photoProtuneCode="21"
multiShotProtuneCode="34"

goProGetPossibleProtune[]:=spotMeterPossible

goProSetProtune[param_String]:=(
	execute[goProMakeCommand["setting",videoProtuneCode,spotMeterToCode[[ToLowerCase[param] ]]]];
	execute[goProMakeCommand["setting",photoProtuneCode,spotMeterToCode[[ToLowerCase[param] ]]]];
	execute[goProMakeCommand["setting",multiShotProtuneCode,spotMeterToCode[[ToLowerCase[param] ]]]])

goProSetVideoProtune[param_String]:=execute[goProMakeCommand["setting",videoProtuneCode,spotMeterToCode[[ToLowerCase[param] ]]]]
goProSetPhotoProtune[param_String]:=execute[goProMakeCommand["setting",photoProtuneCode,spotMeterToCode[[ToLowerCase[param] ]]]]
goProSetMultiShotProtune[param_String]:=execute[goProMakeCommand["setting",multiShotProtuneCode,spotMeterToCode[[ToLowerCase[param] ]]]]

goProSwitchProtuneOn[]:=goProSetProtune["on"]
goProSwitchProtuneOff[]:=goProSetProtune["off"]


(* ::Subsection:: *)
(* Shutter *)
goProShutter[] :=
    execute[goProMakeCommand["command","shutter?p=1"]]
goProStop[] :=
    execute[goProMakeCommand["command","shutter?p=0"]]
    


(* ::Subsection:: *)
(* WhiteBalance *)
wbPhotoCode="22"
wbVideoCode="11"
wbMultiCode="35"
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
    execute[goProMakeCommand["setting",wbPhotoCode,wbToCode[[ToLowerCase[param] ]]]]

goProSetVideoWhiteBalance[param_String] :=
    execute[goProMakeCommand["setting",wbVideoCode,wbToCode[[ToLowerCase[param] ]]]]	
    

goProSetMultiShotWhiteBalance[param_String] :=
    execute[goProMakeCommand["setting",wbMultiCode,wbToCode[[ToLowerCase[param] ]]]]	
    
    

(* ::Subsection:: *)
(* Collor Profile *)
(*Color Profile *)

coVideoCode="12"
coPhotoCode="23"
coMultiCode="36"
coToCode = <|
    "gopro"->"0",
    "flat"->"1"
|>;
coPossible = {"GoPro","Flat"};
goProGetPossibleColorProfile[] :=coPossible
goProSetVideoColorProfile[param_String] := execute[goProMakeCommand["setting",coVideoCode,coToCode[[ToLowerCase[param] ]]]]
goProSetPhotoColorProfile[param_String] := execute[goProMakeCommand["setting",coPhotoCode,coToCode[[ToLowerCase[param] ]]]]
goProSetMultiShotColorProfile[param_String] :=execute[goProMakeCommand["setting",coMultiCode,coToCode[[ToLowerCase[param] ]]]]	


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

isoLimitVideoCode="13"
isoLimitPhotoCode="24"
isoLimitMultiCode="37"


goProGetPossibleVideoIsoLimit[]:=isoLimitVideoPossible
goProGetPossiblePhotoIsoLimit[]:=isoLimitPhotoPossible
goProGetPossibleMultiShotIsoLimit[]:=isoLimitMultiPossible

goProSetVideoIsoLimit[param_String]:=execute[goProMakeCommand["setting",isoLimitVideoCode,isoLimitVideoToCode[[ToLowerCase[param] ]]]]
goProSetPhotoIsoLimit[param_String]:=execute[goProMakeCommand["setting",isoLimitPhotoCode,isoLimitPhotoToCode[[ToLowerCase[param] ]]]]
goProSetMultiShotIsoLimit[param_String]:=execute[goProMakeCommand["setting",isoLimitMultiCode,isoLimitMultiToCode[[ToLowerCase[param] ]]]]


(* ::Subsection:: *)
(* ISO MODE *)
isoModeCode="74"
isoModePossible={"max","lock"}
isoModeToCode=<|"max"->"0","lock"->"1"|>

goProSetIsoMode[param_String]:=execute[goProMakeCommand["setting",isoModeCode,isoModeToCode[[ToLowerCase[param] ]]]]
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

isoMinPhotoCode="75"
isoMinMultiCode="76"

goProSetPhotoIsoMinimum[param_String]:=execute[goProMakeCommand["setting",isoMinPhotoCode,isoMinToCode[[ToLowerCase[param] ]]]]
goProGetPossiblePhotoIsoMinimum[]:=isoMinPossible

goProSetMultiShotIsoMinimum[param_String]:=execute[goProMakeCommand["setting",isoMinMultiCode,isoMinToCode[[ToLowerCase[param] ]]]]
goProGetPossibleMultiShotIsoMinimum[]:=isoMinPossible


(* ::Subsection:: *)
(* Sharpness *)
sharpnessPossible={"high","medium","low"};

sharpnessToCode=<|"high"->"0","medium"->"1","low"->"2"|>

videoSharpnessCode="14"
photoSharpnessCode="25"
multiShotSharpnessCode="38"

goProGetPossibleSharpness[]:=sharpnessPossible

goProSetVideoSharpness[param_String]:=execute[goProMakeCommand["setting",videoSharpnessCode,sharpnessToCode[[ToLowerCase[param] ]]]]
goProSetPhotoSharpness[param_String]:=execute[goProMakeCommand["setting",photoSharpnessCode,sharpnessToCode[[ToLowerCase[param] ]]]]
goProSetMultiShotSharpness[param_String]:=execute[goProMakeCommand["setting",multiShotSharpnessCode,sharpnessToCode[[ToLowerCase[param] ]]]]



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
videoResCode="2"

videoResPossibleBlack=Keys[videoResToCodeBlack];
videoResPossibleSilver=Keys[videoResToCodeSilver];
videoResPossibleSession=Keys[videoResToCodeSession];


goProGetPossibleVideoResolution[]:=Switch[model,"Black",videoResPossibleBlack,
	"Silver",videoResPossibleSilver,
	"Session",videoResPossibleSession,
	_, Message[goProSetCameraModel::model, model]
	]
goProSetVideoResolution[param_String]:=Switch[model,"Black",execute[goProMakeCommand["setting",videoResCode,videoResToCodeBlack[[param]]]],
	"Silver",execute[goProMakeCommand["setting",videoResCode,videoResToCodeSilver[[param]]]],
	"Session",execute[goProMakeCommand["setting",videoResCode,videoResToCodeSession[[param]]]],
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
"25"->"9",
"24"->"10",
"15"->"11",
"12.5"->"12"
|>

fpsPossibleBlack=<|
(*"4K"*)"1"->{"30","25","24"},
(*"4K SuperView"*)"2"->{"24"},
(*"2.7K"*)"4"->{"60","50","48","30","25","24"},
(*"2.7K SuperView"*)"5"->{"30","25"},
(*"2.7K 4:3"*)"6"->{"30","25"},
(*"1440p"*)"7"->{"80","60","50","48","30","25","24"},
(*"1080p SuperView"*)"8"->{"80","60","50","48","30","25","24"},
(*"1080p"*)"9"->{"120","90","60","50","48","30","25","24"},
(*"960p"*)"10"->{"120","60","50"},
(*"720p SuperView"*)"11"->{"120","60","50"},
(*"720p"*)"12"->{"240","120","60","50","30","25"},
(*"WVGA"*)"13"->{"240"}
|>

fpsPossibleSession=<|
(*"1440p"*)"7"->{"30","25"},
(*"1080p SuperView"*)"8"->{"48","30","25"},
(*"1080p"*)"9"->{"60","50","30","25"},
(*"960p"*)"10"->{"60","50","30","25"},
(*"720p SuperView"*)"11"->{"60","50","30","25"},
(*"720p"*)"12"->{"100","60","50","30","25"},
(*"WVGA"*)"13"->{"120","100"}
|>

fpsPossibleSilver=<|
(*"4K"*)"1"->{"15","12.5"},
(*"2.7K"*)"4"->{"30","25","24"},
(*"1440p"*)"7"->{"48","30","25","24"},
(*"1080p SuperView"*)"8"->{"60","50","48","30","25","24"},
(*"1080p"*)"9"->{"60","50","48","30","25","24"},
(*"960p"*)"10"->{"100","60","50"},
(*"720p SuperView"*)"11"->{"100","60","50"},
(*"720p"*)"12"->{"120","60","50","30","25"},
(*"WVGA"*)"13"->{"240"}
|>

fpsCode="3";

fpsPossible={"240",
"120",
"100",
"90",
"80",
"60",
"50",
"48",
"30",
"25",
"24",
"15",
"12.5"}


goProGetPossibleFPS[]:=fpsPossible

goProGetCurrentlyPossibleFPS[]:=Switch[model,
	"Black", fpsPossibleBlack[[ ToString[getSettings[videoResCode]] ]],
	"Silver",fpsPossibleSilver[[ ToString[getSettings[videoResCode]] ]],
	"Session",fpsPossibleSession[[ ToString[getSettings[videoResCode]] ]],
	_, Message[goProSetCameraModel::model, model]
	]
   
goProGetPossibleFPS[param_String]:=Switch[model,"Black", fpsPossibleBlack[[videoResToCodeBlack[[param]]]],
	"Silver",fpsPossibleSilver[[videoResToCodeSilver[[param]]]],
	"Session",fpsPossibleSession[[videoResToCodeSession[[param]]]],
	_, Message[goProSetCameraModel::model, model]
	]

	
goProSetFPS[param_String]:=Switch[model,"Black",execute[goProMakeCommand["setting",fpsCode,fpsToCode[[param]]]],
	"Silver",execute[goProMakeCommand["setting",fpsCode,fpsToCode[[param]]]],
	"Session",execute[goProMakeCommand["setting",fpsCode,fpsToCode[[param]]]],
	_, Message[goProSetCameraModel::model, model]
	]




(* ::Subsection:: *)
(* Shutter Time *)
shutterTimeToCode=<|
	"auto"->"0",
	"1/24"->"3",
	"1/30"->"5",
	"1/48"->"6",
	"1/60"->"8",
	"1/90"->"10",
	"1/96"->"11",
	"1/120"->"13",
	"1/180"->"15",
	"1/192"->"16",
	"1/240"->"18",
	"1/360"->"20",
	"1/480"->"22",
	"1/960"->"23"
|>
shutterTimeCode="73"
shutterTimeFPSPossible=<|
(*"240"*)"0"->{"auto","1/240","1/480","1/960"},
(*"120"*)"1"->{"auto","1/120","1/240","1/480"},
(*"100"*)"2"->{},
(*"90"*)"3"->{"auto","1/90","1/180","1/360"},
(*"80"*)"4"->{},
(*"60"*)"5"->{"auto","1/60","1/120","1/240"},
(*"50"*)"6"->{},
(*"48"*)"7"->{"auto","1/48","1/96","1/192"},
(*"30"*)"8"->{"auto","1/30","1/60","1/120"},
(*"25"*)"9"->{},
(*"24"*)"10"->{"auto","1/24","1/48","1/96"},
(*"15"*)"11"->{},
(*"12.5"*)"12"->{}
|>

shutterTimePossible=Keys[shutterTimeToCode]
goProGetPossibleShutterTime[]:=shutterTimePossible
goProGetPossibleShutterTime[param_String]:=shutterTimeFPSPossible[[fpsToCode[[param]]]]
goProSetShutterTime[param_String]:=execute[goProMakeCommand["setting",shutterTimeCode,shutterTimeToCode[[param]]]]
goProGetCurrentlyPossibleShutterTimeFPS[]:= shutterTimeFPSPossible[[ ToString[getSettings[fpsCode]] ]]





(* ::Subsection:: *)
(* FOV *)
fovCode="4"
fovToCode=<|
	"wide"->"0",
	"medium"->"1",
	"narrow"->"2",
	"linear"->"4"
|>
fovPossible={"wide","medium","narrow","linear"}
goProGetPossibleFOV[]:=fovPossible
goProSetFOV[param_String]:=execute[goProMakeCommand["setting",fovCode,fovToCode[[param]]]]



(* ::Subsection:: *)
(* LowLight *)
lowLightCode="8"
lowLightToCode=<|"on"->"1","off"->"0"|>
lowLightPossible={"on","off"}
goProSetLowLight[param_String]:=execute[goProMakeCommand["setting",lowLightCode,lowLightToCode[[param]]]]
goProGetPossibleLowLight[]:=lowLightPossible
goProSwitchLowLightOn[]:=goProSetLowLight["on"]
goProSwitchLowLightOff[]:=goProSetLowLight["off"]


(* ::Subsection:: *)
(* Spot Meter *)
videoSpotMeterCode="9"
spotMeterToCode=<|"on"->"1","off"->"0"|>
spotMeterPossible={"on","off"}
goProSetVideoSpotMeter[param_String]:=execute[goProMakeCommand["setting",videoSpotMeterCode,spotMeterToCode[[param]]]]
goProGetPossibleVideoSpotMeter[]:=spotMeterPossible
goProSwitchVideoSpotMeterOn[]:=goProSetVideoSpotMeter["on"]
goProSwitchVideoSpotMeterOff[]:=goProSetVideoSpotMeter["off"]





(* ::Subsection:: *)
(* Video Loop *)
videoLoopCode="6"
videoLoopToCode = <|
   "5"->"1",
   "20"->"2",
   "60"->"3",
   "120"->"4",
   "max"->"0"
   |>;
videoLoopPossible = {"max","5","20","60","120"}
goProGetPossibleVideoLoop[] :=videoLoopPossible
goProSetVideoLoop[param_String]:=execute[goProMakeCommand["setting",videoLoopCode,videoLoopToCode[[param]]]]



(* ::Subsection:: *)
(* PhotonVideo *)
photoInVideoCode="7"
photoInVideoToCode = <|
    "5"->"1",
    "10"->"2",
    "30"->"3",
    "60"->"4"
|>

photoInVideoPossible = {"5","10","30","60"}
goProGetPossiblePhotoInVideo[] :=photoInVideoPossible
goProSetPhotoInVideo[param_String] :=execute[goProMakeCommand["setting",photoInVideoCode,photoInVideoToCode[[param]]]]





(* ::Subsection:: *)
(* Photo Resolution *)

photoResCode="17"
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

 
goProSetPhotoRes[param_String] := execute[goProMakeCommand["setting",photoResCode,photoResToCode[[param]]]]
goProGetPossiblePhotoRes[] :=photoResPossible



(* ::Subsection:: *)
(* NightPhotoExposureTime *)
nightPhotoExTimeCode="19"
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
goProSetNightPhotoExposureTime[param_String]:=execute[goProMakeCommand["setting",nightPhotoExTimeCode,nightPhotoExTimeToCode[[param]]]]


(* ::Subsection:: *)
(* Photo Spot Meter *)
photoSpotMeterCode="20"
goProSetPhotoSpotMeter[param_String]:=execute[goProMakeCommand["setting",photoSpotMeterCode,spotMeterToCode[[param]]]]
goProGetPossiblePhotoSpotMeter[]:=spotMeterPossible
goProSwitchPhotoSpotMeterOn[]:=goProSetPhotoSpotMeter["on"]
goProSwitchPhotoSpotMeterOff[]:=goProSetPhotoSpotMeter["off"]

goProSwitchSpotMeterOn[]:=(goProSetPhotoSpotMeter["on"];goProSetVideoSpotMeter["on"];goProSetMultiShotSpotMeter["on"])
goProSwitchSpotMeterOff[]:=(goProSetPhotoSpotMeter["off"];goProSetVideoSpotMeter["off"];goProSetMultiShotSpotMeter["off"])




(* ::Subsection:: *)
(* ContinuousPhoto *)
continuousShotCode="18"
continuousShotToCode = <|
   "3"->"0",
   "5"->"1",
   "10"->"2"
   |>;
continuousShotPossible = {"3","5","10"}
goProGetPossibleContinuousShot[] :=continuousShotPossible
goProSetContinuousShot[param_String] :=execute[goProMakeCommand["setting",continuousShotCode,continuousShotToCode[[param]]]]




(* ::Subsection:: *)
(* NightLapseExposureTime *)

nightLapseExTimeCode="31"
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
goProSetNightLapseExposureTime[param_String]:=execute[goProMakeCommand["setting",nightLapseExTimeCode,nightLapseExTimeToCode[[param]]]]

	
	

(* ::Subsection:: *)
(* NightLapseTimeInterval *)
nightLapseIntervalCode="32"
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
goProSetNightLapseInterval[param_String]:=execute[goProMakeCommand["setting",nightLapseIntervalCode,nightLapseIntervalToCode[[param]]]]
	
	
	
	

(* ::Subsection:: *)
(* MultiShotresolution *)
multiShotResolutionCode="28"
goProSetMultiShotResolution[param_String]:=execute[goProMakeCommand["setting",multiShotResolutionCode,photoResToCode[[param]]]]
goProGetPossibleMultiShotResolution[]:=photoResPossible
	


(* ::Subsection:: *)
(* TimeLapse Interval *)
multiShotTimeLapseIntervalCode="30"
videoTimeLapseIntervalCode="5"
multiShotTimeLapseIntervalToCode=<|"0.5"->"0",
"1"->"1",
"2"->"2",
"5"->"5",
"10"->"10",
"30"->"30",
"60"->"60"
|>
videoTimeLapseIntervalToCode=<|
"0.5"->"0",
"1"->"1",
"2"->"2",
"5"->"3",
"10"->"4",
"30"->"5",
"60"->"6"
|>
timeLapseIntervalPossible={"0.5","1","2","5","10","30","60"}
goProSetMultiShotTimeLapseInterval[param_String]:=execute[goProMakeCommand["setting",multiShotTimeLapseIntervalCode,multiShotTimeLapseIntervalToCode[[param]]]]
goProSetVideoTimeLapseInterval[param_String]:=execute[goProMakeCommand["setting",videoTimeLapseIntervalCode,videoTimeLapseIntervalToCode[[param]]]]
goProGetPossibleTimeLapseInterval[]:=timeLapseIntervalPossible
goProSetTimeLapseInterval[param_String]:=(goProSetVideoTimeLapseInterval[param];goProSetMultiShotTimeLapseInterval[param])


(* ::Subsection:: *)
(* Photo Spot Meter *)
multiShotSpotMeterCode="33"
goProSetMultiShotSpotMeter[param_String]:=execute[goProMakeCommand["setting",multiShotSpotMeterCode,spotMeterToCode[[param]]]]
goProGetPossibleMultiShotSpotMeter[]:=spotMeterPossible
goProSwitchMultiShotSpotMeterOn[]:=goProSetMultiShotSpotMeter["on"]
goProSwitchMultiShotSpotMeterOff[]:=goProSetMultiShotSpotMeter["off"]




(* ::Subsection:: *)
(* Burst Rate *)
burstRateCode="29"
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
goProSetBurstRate[param_String] := execute[goProMakeCommand["setting",burstRateCode,burstRateToCode[[param]]]]




(* ::Subsection:: *)
(* Orientation *)
orientationCode="52"
orientationPossible = {"up","down","gyro"}
orientationToCode=<|"up"->"1","down"->"2","gyro"->"0"|>
goProSetOrientation[param_String]:=execute[goProMakeCommand["setting",orientationCode,orientationToCode[[param]]]]
goProGetPossibleOrientation[]:=orientationPossible



(* ::Subsection:: *)
(* Quick Capture *)
quickCaptureCode="54"
goProGetPossibleQuickCapture[]:=spotMeterPossible
goProSetQuickCapture[param_String]:=execute[goProMakeCommand["setting",quickCaptureCode,spotMeterToCode[[param]]]]
goProSwitchQuickCaptureOn[]:=goProSetQuickCapture["on"]
goProSwitchQuickCaptureOff[]:=goProSetQuickCapture["off"]



(* ::Subsection:: *)
(* LED *)
ledCode="55"
ledToCode = <|
    "off"->"0",
    "2"->"1",
    "4"->"2"
|>
ledPossible = {"off","2","4"}
goProGetPossibleLed[] := ledPossible
goProSetLed[param_String] := execute[goProMakeCommand["setting",ledCode,ledToCode[[param]]]]



(* ::Subsection:: *)
(* Volume *)
volumeCode="56"
volumeToCode = <|
    "off"->"2",
    "70"->"1",
    "100"->"0"
|>
volumePossible = {"off","70","100"}
goProGetPossibleVolume[] :=volumePossible
goProSetVolume[param_String] := execute[goProMakeCommand["setting",volumeCode,volumeToCode[[param]]]]
 




(* ::Subsection:: *)
(* VideoMode *)
videoModeCode="57"
videoModeToCode = <|
    "ntsc"->"0",
    "pal"->"1"
|>
videoModePossible = {"ntsc","pal"}
goProGetPossibleVideoMode[] := videoModePossible
goProSetVideoMode[param_String] :=execute[goProMakeCommand["setting",videoModeCode,videoModeToCode[[param]]]]


(* ::Subsection:: *)
(* LCD Display *)
lcdDisplayCode="72"
goProSwitchLCDDisplayOn[]:=execute[goProMakeCommand["setting",lcdDisplayCode,"1"]]
goProSwitchLCDDisplayOff[]:=execute[goProMakeCommand["setting",lcdDisplayCode,"0"]]



(* ::Subsection:: *)
(* LCD display Brightness *)
(*lcdBrightness="49"
lcdBrightnessToCode=<|"high"->"0","medium"->"1","low"->"2"|>
lcdBrightnessPossible={"high","medium","low"}
goProSetLCDBrightness[param_String]:=execute[goProMakeCommand["setting",lcdBrightness,lcdBrightnessToCode[[param]]]]
goProGetPossibleLCDBrightness[]:=lcdBrightnessPossible*)



(* ::Subsection:: *)
(* Auto off	 *)
autoOffCode="59"
autoOffToCode=<|"never"->"0",
"1"->"1",
"2"->"2",
"3"->"3",
"5"->"4"|>
autoOffPossible={"never","1","2","3","5"}
goProSetAutoPowerOff[param_String]:=execute[goProMakeCommand["setting",autoOffCode,autoOffToCode[[param]]]]
goProGetPossibleAutoPowerOff[]:=autoOffPossible



(* ::Subsection:: *)
(* Locate *)

goProLocate[] :=execute[goProMakeCommand["command/system","locate?p=1"]]
goProLocateStop[] :=execute[goProMakeCommand["command/system","locate?p=0"]]



(* ::Subsection:: *)
(* Delete *)
goProDeleteLastFile[]:=execute[goProMakeCommand["command/storage/delete","last"]]
goProDeleteAll[]:=execute[goProMakeCommand["command/storage/delete","all"]]





(* ::Subsection:: *)
(* exposure *)
(*exposure*)
evVideoCode="15"
evPhotoCode="26"
evMultiShotCode="39"
evToCode = <|
    "-2.0"->"8",
     "-1.5"->"7",
     "-1.0"->"6",
     "-0.5"->"5",
     "0"->"4",
     "+0.5"->"3",
     "+1.0"->"2",
     "+1.5"->"1",
     "+2.0"->"0"
|>
evPossible = { "0","-2.0", "-1.5", "-1.0", "-0.5", "+0.5", "+1.0", "+1.5", "+2.0"}
goProGetPossibleExposure[] := evPossible
goProSetExposure[param_String] :=(goProSetVideoExposure[param];goProSetPhotoExposure[param];goProSetMultiShotExposure[param])
goProSetVideoExposure[param_String] :=execute[goProMakeCommand["setting",evVideoCode,evToCode[[param]]]]
goProSetPhotoExposure[param_String] :=execute[goProMakeCommand["setting",evPhotoCode,evToCode[[param]]]]
goProSetMultiShotExposure[param_String] :=execute[goProMakeCommand["setting",evMultiShotCode,evToCode[[param]]]]
	






battery=""
currentMode=""
currentSubMode=""
photoRemaining=""
videoRemaining=""
photoBatch=""
numberOfVideos=""
numberOfPhotos=""
recording=""

(* ::Section:: *)
(* Status *)
getCameraStatus[param_String]:= Association[
   URLExecute[HTTPRequest["http://10.5.5.9/gp/gpControl/status"]][[
     1]][[2]]][[param]]


codeTobattery=<|"3"->"full","2"->"half","1"->"low","4"->"charging"|>

codeTocurrentMode=<|"0"->"Video","1"->"Photo","2"->"MultiShot"|>

codeTocurrentSubMode=
<|"0"->
	<|"0"->"Video","1"->"TimeLapse Video","2"->"Photo in Video","3"->"Looping"|>,
"1"->
	<|"0"->"Single Picture","1"->"Continuous","2"->"Night Photo"|>,
"2"->
	<|"0"->"Burst","1"->"TimeLapse","2"->"NightLapse"|>
|>

recordingDecode=<|"0"->"No","1"->"Yes"|>

goProGetCameraStatus[]:=(
recording=recordingDecode[[ToString[ getCameraStatus["8"] ]]];
battery=codeTobattery[[ToString[ getCameraStatus["2"] ]]];
currentMode=codeTocurrentMode[[ToString[ getCameraStatus["43"] ]]];
currentSubMode=codeTocurrentSubMode[[ ToString[ getCameraStatus["43"]] ]][[ ToString[ getCameraStatus["44"] ] ]];
photoRemaining=getCameraStatus["34"];
videoRemaining=getCameraStatus["35"];
photoBatch=getCameraStatus["36"];
numberOfVideos=getCameraStatus["39"];
numberOfPhotos=getCameraStatus["38"];

Print["Recording: "<>recording];
Print["Baterry Level: "<> battery];
Print["Camera Mode: "<> currentMode <>" - "<> currentSubMode];
Print["Remaining Photos: "<> ToString[photoRemaining]];
Print["Remaining Video time: " <>ToString[videoRemaining] <> "s"];
Print["Number of Photo Batches taken: "<>ToString[photoBatch]];
Print["Number of Photo taken: "<>ToString[numberOfPhotos]];
Print["Number of Video taken: "<>ToString[numberOfVideos]];
)

settings=""

settingsInit[]:=settings=URLExecute[HTTPRequest["http://10.5.5.9/gp/gpControl/status"]]
getSettings[param_String]:=Association[settings[[2]][[2]]][[param]]
	

videoSubModeSetting=""
videoResSetting=""
fpsSetting=""
fovSetting=""
videoTimeLapseIntervalSetting=""
videoLoopSetting=""
photoInVideoSetting=""
lowLightSetting=""
videoSpotMeterSetting=""
videoProtuneSetting=""
videoWhiteBalanceSetting=""
videoColorProfileSetting=""
videoSharpnessSetting=""
videoExposureSetting=""
isoModeSetting=""
videoIsoLimitSetting=""
videoModeSetting=""
shutterTimeSetting=""

photoSubModeSetting=""
continuousShotSetting=""
photoResSetting=""
photoSpotMeterSetting=""
photoProtuneSetting=""
photoWhiteBalanceSetting=""
photoColorProfileSetting=""
photoSharpnessSetting=""
photoExposureSetting=""
photoIsoLimitSetting=""
photoIsoMinSetting=""


multiShotSubModeSetting=""
nightPhotoExTimeSetting=""
multiShotSpotMeterSetting=""
multiShotProtuneSetting=""
multiShotWhiteBalanceSetting=""
multiShotColorProfileSetting=""
multiShotSharpnessSetting=""
multiShotExposureSetting=""
multiShotIsoLimitSetting=""
multiShotIsoMinSetting=""
nightLapseExTimeSetting=""
burstRateSetting=""
multiShotTimeLapseIntervalSetting=""
nightLapseIntervalSetting=""
multiShotResSetting=""

volumeSetting=""
autoOffSetting=""
ledSetting=""

codeTovideoResolution=Association[videoResToCodeBlack[[#]] -> # & /@ Keys[videoResToCodeBlack]];
codeToFPS=Association[fpsToCode[[#]] -> # & /@ Keys[fpsToCode]];
codeToFOV=Association[fovToCode[[#]] -> # & /@ Keys[fovToCode]];
codeToVideoTimeLapseInterval=Association[videoTimeLapseIntervalToCode[[#]] -> # & /@ Keys[videoTimeLapseIntervalToCode]];
codeTovideoLoop=Association[videoLoopToCode[[#]] -> # & /@ Keys[videoLoopToCode]];
codeToPhotoInVideo=Association[photoInVideoToCode[[#]] -> # & /@ Keys[photoInVideoToCode]];
codeToLowLight=Association[lowLightToCode[[#]] -> # & /@ Keys[lowLightToCode]];
codeToSpotMeter=Association[spotMeterToCode[[#]] -> # & /@ Keys[spotMeterToCode]];
codeToProtune=codeToSpotMeter
codeToWhiteBalance=Association[wbToCode[[#]] -> # & /@ Keys[wbToCode]];
codeToColorProfile=Association[coToCode[[#]] -> # & /@ Keys[coToCode]];
codeToSharpness=Association[sharpnessToCode[[#]] -> # & /@ Keys[sharpnessToCode]];
codeToExposure=Association[evToCode[[#]] -> # & /@ Keys[evToCode]];
codeToIsoMode=Association[isoModeToCode[[#]] -> # & /@ Keys[isoModeToCode]];
codeToVideoIsoLimit=Association[isoLimitVideoToCode[[#]] -> # & /@ Keys[isoLimitVideoToCode]];
codeToVideoMode=Association[videoModeToCode[[#]] -> # & /@ Keys[videoModeToCode]];

codeToPhotoRes=Association[photoResToCode[[#]] -> # & /@ Keys[photoResToCode]];
codeToNightPhotoExTime=Association[nightPhotoExTimeToCode[[#]] -> # & /@ Keys[nightPhotoExTimeToCode]];
codeToContinuousShot=Association[continuousShotToCode[[#]] -> # & /@ Keys[continuousShotToCode]];
codeToIsoMin=Association[isoMinToCode[[#]] -> # & /@ Keys[isoMinToCode]];
codeToPhotoIsoLimit=Association[isoLimitPhotoToCode[[#]] -> # & /@ Keys[isoLimitPhotoToCode]];
codeToMultiShotIsoLimit=Association[isoLimitMultiToCode[[#]] -> # & /@ Keys[isoLimitMultiToCode]];
codeToNightLapseExTime=Association[nightLapseExTimeToCode[[#]] -> # & /@ Keys[nightLapseExTimeToCode]];
codeToBurstRate=Association[burstRateToCode[[#]] -> # & /@ Keys[burstRateToCode]];
codeToMultiShotTimeLapseInterval=Association[multiShotTimeLapseIntervalToCode[[#]] -> # & /@ Keys[multiShotTimeLapseIntervalToCode]];
codeToNightLapseInterval=Association[nightLapseIntervalToCode[[#]] -> # & /@ Keys[nightLapseIntervalToCode]];

codeToVolume=Association[volumeToCode[[#]] -> # & /@ Keys[volumeToCode]];
codeToAutoOff=Association[autoOffToCode[[#]] -> # & /@ Keys[autoOffToCode]];
codeToLed=Association[ledToCode[[#]] -> # & /@ Keys[ledToCode]];


codeToShutterTime=Association[shutterTimeToCode[[#]] -> # & /@ Keys[shutterTimeToCode]];

downloadVideoSetting[]:=(
	settingsInit[];
	videoSubModeSetting=codeTocurrentSubMode[[ "0" ]][[ ToString[getSettings["68"]] ]];
	videoResSetting=codeTovideoResolution[[ToString[getSettings[videoResCode]]]];
	fpsSetting=codeToFPS[[ToString[getSettings[fpsCode]]]];
	fovSetting=codeToFOV[[ToString[getSettings[fovCode]]]];
	videoTimeLapseIntervalSetting=codeToVideoTimeLapseInterval[[ToString[getSettings[videoTimeLapseIntervalCode]]]];
	videoLoopSetting=codeTovideoLoop[[ ToString[getSettings[videoLoopCode]] ]];
	photoInVideoSetting=codeToPhotoInVideo[[ ToString[ToString[getSettings[photoInVideoCode]]] ]];
	lowLightSetting=codeToLowLight[[ ToString[getSettings[lowLightCode]] ]];
	videoSpotMeterSetting=codeToSpotMeter[[ ToString[getSettings[videoSpotMeterCode]] ]];
	videoProtuneSetting=codeToProtune[[ ToString[getSettings[videoProtuneCode]] ]];
	videoWhiteBalanceSetting=codeToWhiteBalance[[ ToString[getSettings[wbVideoCode]] ]];
	videoColorProfileSetting=codeToColorProfile[[ ToString[getSettings[coVideoCode]] ]];
	videoSharpnessSetting=codeToSharpness[[ ToString[getSettings[videoSharpnessCode]] ]];
	videoExposureSetting=codeToExposure[[ ToString[getSettings[evVideoCode]] ]];
	isoModeSetting=codeToIsoMode[[ ToString[getSettings[isoModeCode]] ]];
	videoIsoLimitSetting=codeToVideoIsoLimit[[ ToString[getSettings[isoLimitVideoCode]] ]];
	videoModeSetting=codeToVideoMode[[ ToString[getSettings[videoModeCode]] ]];
	shutterTimeSetting=codeToShutterTime[[ ToString[getSettings[shutterTimeCode]] ]];
	
)

downloadPhotoSetting[]:=(
	settingsInit[];
	photoSubModeSetting=codeTocurrentSubMode[[ "1" ]][[ ToString[getSettings["69"]] ]];
	continuousShotSetting=codeToContinuousShot[[ ToString[getSettings[continuousShotCode]] ]];
	photoResSetting=codeToPhotoRes[[ ToString[getSettings[photoResCode]] ]];
	nightPhotoExTimeSetting=codeToNightPhotoExTime[[ ToString[getSettings[nightPhotoExTimeCode]] ]];
	photoSpotMeterSetting=codeToSpotMeter[[ ToString[getSettings[photoSpotMeterCode]] ]];
	photoProtuneSetting=codeToProtune[[ ToString[getSettings[photoProtuneCode]] ]];
	photoWhiteBalanceSetting=codeToWhiteBalance[[ ToString[getSettings[wbPhotoCode]] ]];
	photoColorProfileSetting=codeToColorProfile[[ ToString[getSettings[coPhotoCode]] ]];
	photoSharpnessSetting=codeToSharpness[[ ToString[getSettings[photoSharpnessCode]] ]];
	photoExposureSetting=codeToExposure[[ ToString[getSettings[evPhotoCode]] ]];
	photoIsoLimitSetting=codeToPhotoIsoLimit[[ ToString[getSettings[isoLimitPhotoCode]] ]];
	photoIsoMinSetting=codeToIsoMin[[ ToString[getSettings[isoMinPhotoCode]] ]];
	
)

downloadMultiShotSetting[]:=(
	settingsInit[];
	multiShotSubModeSetting=codeTocurrentSubMode[[ "2" ]][[ ToString[getSettings["70"]] ]];
	multiShotSpotMeterSetting=codeToSpotMeter[[ ToString[getSettings[multiShotSpotMeterCode]] ]];
	multiShotProtuneSetting=codeToProtune[[ ToString[getSettings[photoProtuneCode]] ]];
	multiShotWhiteBalanceSetting=codeToWhiteBalance[[ ToString[getSettings[wbMultiCode]] ]];
	multiShotColorProfileSetting=codeToColorProfile[[ ToString[getSettings[coMultiCode]] ]];
	multiShotSharpnessSetting=codeToSharpness[[ ToString[getSettings[multiShotSharpnessCode]] ]];
	multiShotExposureSetting=codeToExposure[[ ToString[getSettings[evMultiShotCode]] ]];
	multiShotIsoLimitSetting=codeToMultiShotIsoLimit[[ ToString[getSettings[isoLimitMultiCode]] ]];
	multiShotIsoMinSetting=codeToIsoMin[[ ToString[getSettings[isoMinMultiCode]] ]];
	nightLapseExTimeSetting=codeToNightLapseExTime[[ ToString[getSettings[nightLapseExTimeCode]] ]];
	burstRateSetting=codeToBurstRate[[ ToString[getSettings[burstRateCode]] ]];
	multiShotTimeLapseIntervalSetting=codeToMultiShotTimeLapseInterval[[ ToString[getSettings[multiShotTimeLapseIntervalCode]] ]];
	nightLapseIntervalSetting=codeToNightLapseInterval[[ ToString[getSettings[nightLapseIntervalCode]] ]];
	multiShotResSetting=codeToPhotoRes[[ ToString[getSettings[multiShotResolutionCode]] ]];
	
	
	
	
)

downloadOtherSetting[]:=(
	settingsInit[];
	volumeSetting=codeToVolume[[ ToString[getSettings[volumeCode]] ]];
	autoOffSetting=codeToAutoOff[[ ToString[getSettings[autoOffCode]] ]];
	ledSetting=codeToLed[[ ToString[getSettings[ledCode]] ]];
)

videoSettingReport[]:=(
	Print["Video Settings:"];
	Print["		Current Mode: "<> videoSubModeSetting];
	Print["		Resolutin: "<>videoResSetting];
	Print["		FPS: "<>fpsSetting];
	Print["		FOV: "<>fovSetting];
	Print["		TimeLapse Interval: "<>videoTimeLapseIntervalSetting<>"s"];
	Print["		VideoLoop Interval: "<>videoLoopSetting <> "m"];
	Print["		Photo In Video Interval: "<>photoInVideoSetting<>"s"];
	Print["		Low Light: "<>lowLightSetting];
	Print["		Spot Meter: "<>videoSpotMeterSetting];
	Print["		Protune: "<>videoProtuneSetting];
	Print["		White Balance: "<>videoWhiteBalanceSetting];
	Print["		Color Profile: "<>videoColorProfileSetting];
	Print["		Sharpness: "<>videoSharpnessSetting];
	Print["		Exposure: "<>videoExposureSetting];
	Print["		Shutter Time: "<>shutterTimeSetting];
	Print["		ISO Mode: "<>isoModeSetting];	
	Print["		ISO Limit: "<>videoIsoLimitSetting];	
	Print["		Video Mode: "<>videoModeSetting];
)

multiShotSettingReport[]:=(
	Print["MultiShot Settings:"];

	Print["		Current Mode: "<> multiShotSubModeSetting];
	Print["		Night Lapse Exposure Time: "<>nightLapseExTimeSetting];
	Print["		Resolution: " <>multiShotResSetting];
	Print["		BurstRate: "<>burstRateSetting];
	Print["		TmeLapseInterval: "<> multiShotTimeLapseIntervalSetting];
	Print["		NightLapseInterval: "<> nightLapseIntervalSetting];
	Print["		Spot Meter: "<>multiShotSpotMeterSetting];
	Print["		Protune: "<>multiShotProtuneSetting];
	Print["		White Balance: "<>multiShotWhiteBalanceSetting];
	Print["		Color Profile: "<>multiShotColorProfileSetting];
	Print["		Sharpness: "<>multiShotSharpnessSetting];
	Print["		Exposure: "<>multiShotExposureSetting];
	Print["		ISO Limit: "<>multiShotIsoLimitSetting];
	Print["		ISO Min: "<>multiShotIsoMinSetting];
)

photoSettingReport[]:=(
	Print["Photo Settings:"];
	Print["		Current Mode: "<> photoSubModeSetting];
	Print["		Resolutin: "<>photoResSetting];
	Print["		Continuous Photo Rate: "<> continuousShotSetting ];
	Print["		Night Photo Exposure time : "<> nightPhotoExTimeSetting ];
	Print["		Spot Meter: "<>photoSpotMeterSetting];
	Print["		Protune: "<>photoProtuneSetting];
	Print["		White Balance: "<>photoWhiteBalanceSetting];
	Print["		Color Profile: "<>photoColorProfileSetting];
	Print["		Sharpness: "<>photoSharpnessSetting];
	Print["		Exposure: "<>photoExposureSetting];
	Print["		ISO Limit: "<>photoIsoLimitSetting];	
	Print["		ISO Min: "<>photoIsoMinSetting];

)

otherSettingReport[]:=(
	Print["Other:"];
	Print["		Volume: "<>volumeSetting];
	Print["		AutoPowerOff: "<>autoOffSetting];
	Print["		Led: "<>ledSetting];

)

goProGetVideoSettingReport[]:=(
	downloadVideoSetting[];
	videoSettingReport[];
)

goProGetPhotoSettingReport[]:=(
	downloadPhotoSetting[];
	photoSettingReport[];
)

goProGetMultiShotSettingReport[]:=(
	downloadMultiShotSetting[];
	multiShotSettingReport[];
)

goProGetSettingReport[]:=(
	goProGetVideoSettingReport[];
	goProGetPhotoSettingReport[];
	goProGetMultiShotSettingReport[];
	downloadOtherSetting[];
	otherSettingReport[];
)
downloadAllSetting[]:=(
	downloadVideoSetting[];
	downloadPhotoSetting[];
	downloadMultiShotSetting[];
	downloadOtherSetting[];
)




goProGetSettingReportAssociation[]:=(downloadAllSetting[];<|
"videoResolution"->videoResSetting,
"fps"->fpsSetting,
"fov"->fovSetting,
"videoTimeLapseInterval"->videoTimeLapseIntervalSetting,
"videoLoop"->videoLoopSetting,
"photoInVideo"->photoInVideoSetting,
"lowLight"->lowLightSetting,
"videoSpotMeter"->videoSpotMeterSetting,
"videoProtune"->videoProtuneSetting,
"videoWhiteBalance"->videoWhiteBalanceSetting,
"videoColorProfile"->videoColorProfileSetting,
"videoSharpness"->videoSharpnessSetting,
"videoExposure"->videoExposureSetting,
"isoMode"->isoModeSetting,
"videoIsoLimit"->videoIsoLimitSetting,
"videoMode"->videoModeSetting,
"shutterTime"->shutterTimeSetting,

"continuousShot"->continuousShotSetting,
"photoResolution"->photoResSetting,
"photoSpotMeter"->photoSpotMeterSetting,
"photoProtune"->photoProtuneSetting,
"photoWhiteBalance"->photoWhiteBalanceSetting,
"photoColorProfile"->photoColorProfileSetting,
"photoSharpness"->photoSharpnessSetting,
"photoExposure"->photoExposureSetting,
"photoIsoLimit"->photoIsoLimitSetting,
"photoIsoMin"->photoIsoMinSetting,
"nightPhotoExposureTime"->nightPhotoExTimeSetting,

"multiShotSpotMeter"->multiShotSpotMeterSetting,
"multiShotProtune"->multiShotProtuneSetting,
"multiShotWhiteBalance"->multiShotWhiteBalanceSetting,
"multiShotColorProfile"->multiShotColorProfileSetting,
"multiShotSharpness"->multiShotSharpnessSetting,
"multiShotExposure"->multiShotExposureSetting,
"multiShotIsoLimit"->multiShotIsoLimitSetting,
"multiShotIsoMin"->multiShotIsoMinSetting,
"nightLapseExposureTime"->nightLapseExTimeSetting,
"burstRate"->burstRateSetting,
"multiShotTimeLapseInterval"->multiShotTimeLapseIntervalSetting,
"nightLapseInterval"->nightLapseIntervalSetting,
"multiShotResolution"->multiShotResSetting,
"volume"->volumeSetting,
"autoOff"->autoOffSetting,
"led"->ledSetting
|>)


vars={
"videoResolution",
"fps",
"fov",
"videoTimeLapseInterval",
"videoLoop",
"photoInVideo",
"lowLight",
"videoSpotMeter",
"videoProtune",
"videoWhiteBalance",
"videoColorProfile",
"videoSharpness",
"videoExposure",
"isoMode",
"videoIsoLimit",
"videoMode",
"shutterTime",

"continuousShot",
"photoResolution",
"photoSpotMeter",
"photoProtune",
"photoWhiteBalance",
"photoColorProfile",
"photoSharpness",
"photoExposure",
"photoIsoLimit",
"photoIsoMin",
"nightPhotoExposureTime",

"multiShotSpotMeter",
"multiShotProtune",
"multiShotWhiteBalance",
"multiShotColorProfile",
"multiShotSharpness",
"multiShotExposure",
"multiShotIsoLimit",
"multiShotIsoMin",
"nightLapseExposureTime",
"burstRate",
"multiShotTimeLapseInterval",
"nightLapseInterval",
"multiShotResolution",
"volume",
"autoOff",
"led"
}

goProGet::missing="This parameter is not usable, try another.";

(*spusteni prikazu exec pomoci HTTPRequest a URLRead*)
(*If[MemberQ[vars,param],Message[goProGet::missing, missing]]*)
SetAttributes[goProGet,HoldAll];

goProGet[param_]:= (If[SameQ[Head[Evaluate[param]],List]||SameQ[Head[Evaluate[param]],String],goProGet[Evaluate[param]],
	If[MemberQ[vars,#],# -> goProGetSettingReportAssociation[][[#]],Message[goProGet::missing, missing]] &/@ {ToString[param]}])

goProGet[list_List]:= If[MemberQ[vars,ToString[#]],ToString[#] -> goProGetSettingReportAssociation[][[ToString[#]]],
	(Print["not member"];Message[goProGet::missing, missing])]&/@ list

goProGet[param_String]:= (If[MemberQ[vars,#],# -> goProGetSettingReportAssociation[][[#]],Message[goProGet::missing, missing]] & /@ {param})

goProGetVariables[]:=ToExpression[#]&/@{
"videoResolution",
"fps",
"fov",
"videoTimeLapseInterval",
"videoLoop",
"photoInVideo",
"lowLight",
"videoSpotMeter",
"videoProtune",
"videoWhiteBalance",
"videoColorProfile",
"videoSharpness",
"videoExposure",
"isoMode",
"videoIsoLimit",
"videoMode",
"shutterTime",

"continuousShot",
"photoResolution",
"photoSpotMeter",
"photoProtune",
"photoWhiteBalance",
"photoColorProfile",
"photoSharpness",
"photoExposure",
"photoIsoLimit",
"photoIsoMin",
"nightPhotoExposureTime",

"multiShotSpotMeter",
"multiShotProtune",
"multiShotWhiteBalance",
"multiShotColorProfile",
"multiShotSharpness",
"multiShotExposure",
"multiShotIsoLimit",
"multiShotIsoMin",
"nightLapseExposureTime",
"burstRate",
"multiShotTimeLapseInterval",
"nightLapseInterval",
"multiShotResolution",
"volume",
"autoOff",
"led"
}


(* ::Subsection:: *)
(* Downlpad *)


urlBase="http://10.5.5.9:8080/videos/DCIM/100GOPRO/";


empty=""
file=""
goProGetFileList::empty="No files on GoPro camera.";
goProGetFileList::file="`1` - No such file on GoPro camera.";
goProGetFileList::directory="`1` - No such directory exists.";


goProGetFileList[]:=(If[SameQ[Import[urlBase], $Failed],
	Message[goProGetFileList::empty,empty]
	,
	files = {};
	files = Flatten[
  		AppendTo[files, 
   		StringReplace[ToString[#] & /@ DeleteCases[ReadList[StringToStream[StringTrim[StringDrop[Import[urlBase], 13]]]],
   			 _Real][[All, 3]], Whitespace -> ""]]]
]

 )
      
goProSetURLBase[param_String]:=urlBase=param;
goProGetURLBase[]:=urlBase;


goProDownloadFile[list_List,dest_String]:=goProDownloadFile[#,dest]&/@list     
goProDownloadFile[name_String,dest_String]:=If[URLRead[HTTPRequest[urlBase<>name]]["StatusCode"]!=200,
		Message[goProGetFileList::file,name],
		If[DirectoryQ[dest<>name],URLDownload[urlBase<>name,dest<>name],
			Message[goProGetFileList::directory,dest]
		]
		
	]


goProDownloadAllFiles[dest_String]:=URLDownload[urlBase<>#,dest<>#]&/@goProGetFileList[]

goProGetFileURL[name_String]:=If[URLRead[HTTPRequest[urlBase<>name]]["StatusCode"]!=200,
	Message[goProGetFileList::file,name],
	urlBase<>name
]















(* ::Subsection:: *)
(* goProSet *)


Setting
Options[goProSet] = {
(*MODE SETTING*)
videoResolution->videoResSetting,
fps->fpsSetting,
fov->fovSetting,
videoTimeLapseInterval->videoTimeLapseIntervalSetting,
videoLoop->videoLoopSetting,
photoInVideo->photoInVideoSetting,
lowLight->lowLightSetting,
videoSpotMeter->videoSpotMeterSetting,
videoProtune->videoProtuneSetting,
videoWhiteBalance->videoWhiteBalanceSetting,
videoColorProfile->videoColorProfileSetting,
videoSharpness->videoSharpnessSetting,
videoExposure->videoExposureSetting,
isoMode->isoModeSetting,
videoIsoLimit->videoIsoLimitSetting,
videoMode->videoModeSetting,
shutterTime->shutterTimeSetting,

continuousShot->continuousShotSetting,
photoResolution->photoResSetting,
photoSpotMeter->photoSpotMeterSetting,
photoProtune->photoProtuneSetting,
photoWhiteBalance->photoWhiteBalanceSetting,
photoColorProfile->photoColorProfileSetting,
photoSharpness->photoSharpnessSetting,
photoExposure->photoExposureSetting,
photoIsoLimit->photoIsoLimitSetting,
photoIsoMin->photoIsoMinSetting,
nightPhotoExposureTime->nightPhotoExTimeSetting,

multiShotSpotMeter->multiShotSpotMeterSetting,
multiShotProtune->multiShotProtuneSetting,
multiShotWhiteBalance->multiShotWhiteBalanceSetting,
multiShotColorProfile->multiShotColorProfileSetting,
multiShotSharpness->multiShotSharpnessSetting,
multiShotExposure->multiShotExposureSetting,
multiShotIsoLimit->multiShotIsoLimitSetting,
multiShotIsoMin->multiShotIsoMinSetting,
nightLapseExposureTime->nightLapseExTimeSetting,
burstRate->burstRateSetting,
multiShotTimeLapseInterval->multiShotTimeLapseIntervalSetting,
nightLapseInterval->nightLapseIntervalSetting,
multiShotResolution->multiShotResSetting,
volume->volumeSetting,
autoOff->autoOffSetting,
led->ledSetting

}


goProSet[OptionsPattern[]] :=(
	downloadVideoSetting[];
	downloadPhotoSetting[];
	downloadMultiShotSetting[];
	downloadOtherSetting[];
	
	(*video*)
	
	If[!SameQ[OptionValue[videoResolution],videoResSetting],goProSetVideoResolution[OptionValue[videoResolution]]];
	If[!SameQ[OptionValue[fps],fpsSetting],goProSetFPS[OptionValue[fps]]];
	If[!SameQ[OptionValue[fov],fovSetting],goProSetFOV[OptionValue[fov]]];
	If[!SameQ[OptionValue[videoTimeLapseInterval],videoTimeLapseIntervalSetting],goProSetVideoTimeLapseInterval[OptionValue[videoTimeLapseInterval]]];
	If[!SameQ[OptionValue[videoLoop],videoLoopSetting],goProSetVideoLoop[OptionValue[videoLoop]]];
	If[!SameQ[OptionValue[photoInVideo],photoInVideoSetting],goProSetPhotoInVideo[OptionValue[photoInVideo]]];
	If[!SameQ[OptionValue[lowLight],lowLightSetting],goProSetLowLight[OptionValue[lowLight]]];
	If[!SameQ[OptionValue[videoSpotMeter],videoSpotMeterSetting],goProSetVideoSpotMeter[OptionValue[videoSpotMeter]]];
	If[!SameQ[OptionValue[videoProtune],videoProtuneSetting],goProSetVideoProtune[OptionValue[videoProtune]]];
	If[!SameQ[OptionValue[videoWhiteBalance],videoWhiteBalanceSetting],goProSetVideoWhiteBalance[OptionValue[videoWhiteBalance]]];
	If[!SameQ[OptionValue[videoColorProfile],videoColorProfileSetting],goProSetVideoColorProfile[OptionValue[videoColorProfile]]];
	If[!SameQ[OptionValue[videoSharpness],videoSharpnessSetting],goProSetVideoSharpness[OptionValue[videoSharpness]]];
	If[!SameQ[OptionValue[videoExposure],videoExposureSetting],goProSetVideoExposure[OptionValue[videoExposure]]];
	If[!SameQ[OptionValue[isoMode],isoModeSetting],goProSetIsoMode[OptionValue[isoMode]]];
	If[!SameQ[OptionValue[videoIsoLimit],videoIsoLimitSetting],goProSetVideoIsoLimit[OptionValue[videoIsoLimit]]];
	If[!SameQ[OptionValue[videoMode],videoModeSetting],goProSetVideoMode[OptionValue[videoMode]]];
	If[!SameQ[OptionValue[shutterTime],shutterTimeSetting],goProSetShutterTime[OptionValue[shutterTime]]];

	(*Photo*)
	If[!SameQ[OptionValue[continuousShot],continuousShotSetting],goProSetContinuousShot[OptionValue[continuousShot]]];
	If[!SameQ[OptionValue[photoResolution],photoResSetting],goProSetPhotoRes[OptionValue[photoResolution]]];
	If[!SameQ[OptionValue[photoSpotMeter],photoSpotMeterSetting],goProSetPhotoSpotMeter[OptionValue[photoSpotMeter]]];
	If[!SameQ[OptionValue[photoProtune],photoProtuneSetting],goProSetPhotoProtune[OptionValue[photoProtune]]];
	If[!SameQ[OptionValue[photoWhiteBalance],photoWhiteBalanceSetting],goProSetPhotoWhiteBalance[OptionValue[photoWhiteBalance]]];
	If[!SameQ[OptionValue[photoColorProfile],photoColorProfileSetting],goProSetPhotoColorProfile[OptionValue[photoColorProfile]]];
	If[!SameQ[OptionValue[photoSharpness],photoSharpnessSetting],goProSetPhotoSharpness[OptionValue[photoSharpness]]];	
	If[!SameQ[OptionValue[photoExposure],photoExposureSetting],goProSetPhotoExposure[OptionValue[photoExposure]]];
	If[!SameQ[OptionValue[photoIsoLimit],photoIsoLimitSetting],goProSetPhotoIsoLimit[OptionValue[photoIsoLimit]]];
	If[!SameQ[OptionValue[photoIsoMin],photoIsoMinSetting],goProSetPhotoIsoMinimum[OptionValue[photoIsoMin]]];
	If[!SameQ[OptionValue[nightPhotoExposureTime],nightPhotoExTimeSetting],goProSetNightPhotoExposureTime[OptionValue[nightPhotoExposureTime]]];
	
	(*MultiShot*)
	If[!SameQ[OptionValue[multiShotSpotMeter],multiShotSpotMeterSetting],goProSetMultiShotSpotMeter[OptionValue[multiShotSpotMeter]]];
	If[!SameQ[OptionValue[multiShotProtune],multiShotProtuneSetting],goProSetMultiShotProtune[OptionValue[multiShotProtune]]];
	If[!SameQ[OptionValue[multiShotWhiteBalance],multiShotWhiteBalanceSetting],goProSetMultiShotWhiteBalance[OptionValue[multiShotWhiteBalance]]];
	If[!SameQ[OptionValue[multiShotColorProfile],multiShotColorProfileSetting],goProSetMultiShotColorProfile[OptionValue[multiShotColorProfile]]];
	If[!SameQ[OptionValue[multiShotSharpness],multiShotSharpnessSetting],goProSetMultiShotSharpness[OptionValue[multiShotSharpness]]];
	If[!SameQ[OptionValue[multiShotExposure],multiShotExposureSetting],goProSetMultiShotExposure[OptionValue[multiShotExposure]]];
	If[!SameQ[OptionValue[multiShotIsoLimit],multiShotIsoLimitSetting],goProSetMultiShotIsoLimit[OptionValue[multiShotIsoLimit]]];
	If[!SameQ[OptionValue[multiShotIsoMin],multiShotIsoMinSetting],goProSetMultiShotIsoMinimum[OptionValue[multiShotIsoMin]]];
	If[!SameQ[OptionValue[nightLapseExposureTime],nightLapseExTimeSetting],goProSetNightLapseExposureTime[OptionValue[nightLapseExposureTime]]];
	If[!SameQ[OptionValue[burstRate],burstRateSetting],goProSetBurstRate[OptionValue[burstRate]]];
	If[!SameQ[OptionValue[multiShotTimeLapseInterval],multiShotTimeLapseIntervalSetting],goProSetMultiShotTimeLapseInterval[OptionValue[multiShotTimeLapseInterval]]];
	If[!SameQ[OptionValue[nightLapseInterval],nightLapseIntervalSetting],goProSetNightLapseInterval[OptionValue[nightLapseInterval]]];
	If[!SameQ[OptionValue[multiShotResolution],multiShotResSetting],goProSetMultiShotResolution[OptionValue[multiShotResolution]]];
	
	(*other*)
	If[!SameQ[OptionValue[volume],volumeSetting],goProSetVolume[OptionValue[volume]]];
	If[!SameQ[OptionValue[autoOff],autoOffSetting],goProSetAutoPowerOff[OptionValue[autoOff]]];	
	If[!SameQ[OptionValue[led],ledSetting],goProSetLed[OptionValue[led]]];
	



)








    
    

End[]


EndPackage[]