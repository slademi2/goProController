(* Wolfram Language Package *)

BeginPackage["GoProController`"]
(* Exported symbols added here with SymbolName::usage *) 



(* ::Section:: *)
(* Global Definitions *)

goProGetModel::usage =
    "goProPrintModel[ ] returns string containing camera model."
    
    


(*get status*)
goProGetStatus::usage =
    "goProGetstatus[unit_String, param_String] Returns value of parameter param"

goProInit::usage = 
    "goProTurnOn[password_ ] user provides password for usage of goProController package"

goProTurnOn::usage = 
    "goProTurnOn[ ] Turns on the gopro camera"

goProTurnOff::usage =    
    "goProTurnOn[ ] Turns off the gopro camera"
    
goProShutter::usage =
    "goProShutter[ ] Start of recording or taking photo on camera"

goProStop::usage =
    "goProStop[ ] Stop the camera recording"
    
goProVideoMode::usage =
    "goProVideoMode[ ] Switch camera to video mode"

goProPhotoMode::usage =
    "goProPhotoMode[ ] Switch camera to photo mode"

goProBurstMode::usage =
    "goProBurstMode[ ] Switch camera to burst mode"
    
goProTimeLapseMode::usage =
    "goProTimeLapseMode[ ] Switch camera to time-lapse mode"
    
(*resolutions settings for photo*)

goProSetPhotoRes::usage =
    "goProSetPhotoRes[_String] Sets Video resolution acording to parameter. To see which parameter you can be used use function goProGetPossiblePhotoRes[]"
goProGetPossiblePhotoRes::usage =
    "goProGetPossiblePhotoRes[ ] Returns list of possible parameters for function goProSetVideoRes[_String]"


goProPhotoRes12W::usage =
    "goProPhotoRes12W[ ] Sets resolution of photo to 12mp and Wide picture"

goProPhotoRes7W::usage =
    "goProPhotoRes7W[ ] Sets resolution of photo to 7mp and Wide picture"

goProPhotoRes7M::usage =
    "goProPhotoRes7M[ ] Sets resolution of photo to 7mp and Medium picture"

goProPhotoRes5M::usage =
    "goProPhotoRes5M[ ] Sets resolution of photo to 5mp and Medium picture"
    
(*video resolutions*)    
goProSetVideoRes::usage =
    "goProSetVideoRes[_String] Sets Video resolution acording to parameter. To see which parameter you can be used use function goProGetPossibleVideoRes[]"
goProGetPossibleVideoRes::usage =
    "goProGetPossibleVideoRes[ ] Returns list of possible parameters for function goProSetVideoRes[_String]"

goProVideoRes4K::usage =
    "goProVideoRes4K[ ]Sets resolution of video to 4K and 15FPS"
goProVideoRes4K17to9::usage =
    "goProVideoRes4K17to9[ ] Sets resolution of video to 4K in 17:9 and 12FPS"
goProVideoRes2point7K17to9::usage =
    "goProVideoRes2point7K17to9[ ]Sets resolution of video to 2.7K in 17:9 and 24FPS"
goProVideoRes2point7K::usage =
    "goProVideoRes2point7K[ ] Sets resolution of video to 2.7K and 30FPS "

goProVideoRes1440::usage =
    "goProVideoRes1440[ ] Sets resolution of video to 1440p and 24FPS "
goProVideoRes1080::usage =
    "goProVideoRes1080[ ] Sets resolution of video to 1080p and 24FPS "
goProVideoRes1080SV::usage =
    "goProVideoRes1080SV[ ] Sets resolution of video to 1080p Super View and 24FPS "
goProVideoRes960::usage =
    "goProVideoRes960[ ] Sets resolution of video to 960p and 48FPS"
goProVideoRes720::usage =
    "goProVideoRes720[ ] Sets resolution of video to 720p and 60FPS"
goProVideoRes720SV::usage =
    "goProVideoRes720SV[ ] Sets resolution of video to 720p Super View 60FPS"
goProVideoResWVGA::usage =
    "goProVideoResWVGA[ ] Sets resolution of video to WVGA 240FPS"
    

(*FPS*)
goProGetPossibleFPS::usage =
    "goProGetPossibleFPS[ ] Returns list of possible FPS for actual video resolution"
goProGetPossibleFPS::usage =
    "goProGetPossibleFPS[ _String] Returns list of possible FPS for video resolution specified by parameter"
goProSetFPS::usage =
    "goProSetFPS[_String] Sets FPS to given parameter, but only if this amount of FPS is supported by actual video resolution.
	 To see which parameter you can use for actual video resolution use goProGetPossibleFPS functuon"
    
    
(*Orientation*)
goProSetOrientationUp::usage =
    "goProSetOrientationUp[ ] Switch orientation of display up"
goProSetOrientationDown::usage =
    "goProSetOrientationDown[ ] Switch orientation of display up"
    
(*Field of view*)
goProSetFOV::usage =
    "goProSetFOV[_String ] Sets FOV to given parameter, accepts only possible parameters. 
	To see which parameter you can use for actual video resolution use goProGetPossibleFOV function" 

goProGetPossibleFOV::usage =
    "goProGetPossibleFOV[ ] Returns list of possible settings for FOV for actual video resolution"

(*TimeLapseInterval*)
goProGetPossibleTimeLapse::usage =
    "goProGetPossibleTimeLapse[ ] Returns possible values of TimeLapse interval in seconds"
goProSetTimeLapse::usage =
    "goProGetTimeLapse[_String] Sets TimeLapse interval to parameter given.
	 To see which parameters you can use for Time Lapse interval use function goProGetPossibleTimeLapse[ ]"
     
     
     
(*Burst Rate*)
goProGetPossibleBurstRate::usage =
    "goProGetPossibleBurstRate[ ] returns possible values of BurstRate"
goProSetBurstRate::usage =
    "goProSetBurstRate[_String] Sets BurstRate to parameter given.
	 To see which parameters you can use for BurstRate use function goProGetPossibleBurstRate[ ]"
     
     
     
(*Video Loop*)
goProGetPossibleVideoLoop::usage =
    "goProGetPossibleVideoLoop[ ] returns possible values of Video Loop"
goProSetVideoLoop::usage =
    "goProSetVideoLoop[_String] Sets Video Loop to parameter given.
	 To see which parameters you can use for Video Loop use function goProGetPossibleVideoLoop[ ]"
     
(*Continuous Shot*)    
goProGetPossibleContinuousShot::usage =
    "goProGetContinuousShot[ ] returns possible values of Continuous Shot"
goProSetContinuousShot::usage =
    "goProSetContinuousShot[ _String] Sets Continuous Shot to parameter given"


(*Photo In Video*)
goProGetPossiblePhotoInVideo::usage =
    "goProGetPossiblePhotoInVideo[ ] returns possible vaules of time in which will camera take photo during video recording."
goProSetPhotoInVideo::usage =
    "goProSetPhotoInVideo[ _String] Sets time interval in which will camera take photo during video recording."  
    

(*Volume*)
goProGetPossibleVolume::usage =
    "goProGetPossibleVolume[ ] returns possible values for volume settings."
goProSetVolume::usage =
    "goProSetVolume[ _String] sets volume to parameter given. For values which are usable see goProGetPossibleVolume[]."
    
    

(*protune*)
goProGetPossibleProtune::usage =
    "goProGetPossibleProtune[ ] returns possible values for protune mode."
goProSetProtune::usage =
    "goProSetProtune[ _String] switch protune mode on/off."
goProSwitchProtuneOn::usage =
    "goProSwitchProtuneOn[ ] Switch protune mode on."
goProSwitchProtuneOff::usage =
    "goProSwitchProtuneOff[ ] Switch protune mode off."
    
    

(*White Balance*)
goProGetPossibleWhiteBalance::usage =
    "goProGetPossibleWhiteBalance[ ] returns possible values for WhiteBalance"
goProSetWhiteBalance::usage =
    "goProSetWhiteBalance[ _String] sets White Balance to parameter given. To see which parameters are supported call goProGetPossibleWhiteBalance[] function. 
		WhiteBalance can be set only if Protune mode is on."


(*Color Profile*)
goProGetPossibleColorProfile::usage =
    "goProGetPossibleColors[ ] returns possible parameters for color profile setting"
goProSetColorProfile::usage =
    "goProSetColorProfile[ _String] sets color profile to parameter given. To see which parameters are supported call goProGetPossibleColorProfile[]. Only if protune mod is on."
    

(*ISO*)
goProGetPossibleISO::usage =
    "goProGetPossibleISO[ ] returns possible parameters for ISO settings"
goProSetISO::usage =
    "goProSetISO[ _String] sets ISO to parameter given. To see which parameters are supported call goProGetPossibleISO[]. Only if protune mod is on."
    
    
(*Sharpness*)
goProGetPossibleSharpness::usage =
    "goProGetPossibleSharpness[ ] returns possible parameters for sharpness settings"
goProSetSharpness::usage =
    "goProSetSharpness[ _String] sets sharpness to parameter given. To see which parameters are supported call goProGetPossibleSharpness[]. Only if protune mod is on."
    

(*Exposure*)
goProGetPossibleExposure::usage =
    "goProGetPossibleExposure[ ] returns possible parameters for exposure compensation settings"
goProSetExposure::usage =
    "goProSetExposure[ _String] sets exposure compensation to parameter given. The range is (-2 to +2), in 0.5 step increments. To see which parameters are supported call goProGetPossibleExposure[]. Only if protune mod is on."


(*LOW Light*)
goProGetPossibleLowLight::usage =
    "goProGetPossibleLowLight[ ] returns possible parameters for LowLight settings"
goProSetLowLight::usage =
    "goProSetLowLight[ _String] sets LowLight to parameter given. To switch low light on you need to have FPS set to 60+. To see which parameters are supported call goProGetPossibleLowLight[]."
goProSwitchLowLightOn::usage =
    "goProSwitchLowLightOn[ ] turns LowLight on if FPS is set to 60+."
goProSwitchLowLightOff::usage =
    "goProSwitchLowLightOff[ ] turns LowLight off."
    

(*Leds*)
goProGetPossibleLed::usage =
    "goProGetPossibleLed[ ] returns possible parameters for Led settings"
goProSetLed::usage =
    "goProSetLed[ _String] sets leds to parameter given. To see which parameter you can use call goProGetPossibleLed[]."
    
    
(*Spot Meter*)
goProGetPossibleSpotMeter::usage =
    "goProGetPossibleSpotMeter[ ] returns possible parameters for SpotMeter settings"
goProSetSpotMeter::usage =
    "goProSetSpotMeter[ _String] sets SpotMeter to parameter given. To see which parameters are supported call goProGetPossibleSpotMeter[]."
goProSetSpotMeterOn::usage =
    "goProSetSpotMeterOn[ ] turns SpotMeter on."
goProSetSpotMeterOff::usage =
    "goProSetSpotMeterOff[ ] turns SpotMeter off."    
    
(*Auto Power Off*)
goProGetPossibleAutoPowerOff::usage =
    "goProGetPossibleAutoPowerOff[ ] returns possible parameters for AutoPowerOff settings"
goProSetAutoPowerOff::usage =
    "goProSetAutoPowerOff[ _String] sets AutoPowerOff to parameter given. To see which parameters are supported call goProGetPossibleAutoPowerOff []."


(*Video Mode*)
goProGetPossibleVideoMode::usage =
    "goProGetPossibleVideoMode[ ] returns possible parameters for VideoMode settings"
goProSetVideoMode::usage =
    "goProSetVideoMode[ _String] sets leds to parameter given. To see which parameter you can use call goProGetPossibleVideoMode[]."

(*LOCATE*)
goProLocate::usage =
    "goProLocate[ ] camera will start sound signal to locate your camera."
goProLocateStop::usage =
    "goProLocateStop[ ] camera will stop sound signal to locate your camera."
    



(*Time*)
goProSetTime::usage =
    "goProSetTime[ _String] sets time to parameter given. Parameter must be in format \"YYMMDDHHMM\", without any spaces or separators."
goProGetTime::usage =
    "goProGetTime[ ] returns string which contains time set on camera."

    
     
(*Settings report*)
goProGetSettingReport::usage =
    "goProGetSettingReport[ ] returns settings of camera"
    
    
    
(*saving settings to file*)
goProSaveSettings::usage =
    "goProSaveSettings[_String] saves settings to the file in parameter."
    

    


    


(*SETTINGS*)
goProSet::usage =
    "goProSet[ ] enables to set camera via rules such as: {fps,fov,videoResolution,photoResolution,...}"

goProSetVideo::usage =
    "goProSetVideo[ ] enables to set video settings such as fps, fov, videoResolution through rules, for example: goProSetVideo[{fps->\"30\",fov->->\"wide\", videoResolution->\"1080p\" }]. All parameters are optional."
    


(* ::Section:: *)
(* Private Definitions *)


Begin["`Private`"]


model = "HERO3+Black";
goProGetModel[] := model




(*adresa kamery na siti, kterou kamera vysila*)
goProUrl = "http://10.5.5.9/";


(*Funkce pro slozeni url pro ovladani kamery, unit (bacpac||camera), command (prikat), parram (parametr prikazu)*)
goProMakeCommand[unit_String, command_String, parram_String] :=
    goProUrl <> unit <> "/" <> command <> "?t=" <> goProPassword <> 
     "&p=%" <> parram

(*spusteni prikazu exec pomoci HTTPRequest a URLRead*)
execute[exec_String] :=
    (request = HTTPRequest[exec];
     URLRead[request])
 

goProGetStatus[unit_String, param_String] :=
    URLExecute[HTTPRequest[goProUrl <> unit <> "/" <> param <> "?t=" <> goProPassword]]

(*Funkce pro inicializaci hesla*)
goProInit[password_String] :=
    (goProPassword = password)

(*funkce pro zapnuti a vypnuti kamery*)
goProTurnOn[] :=
    execute[goProMakeCommand["bacpac", "PW", "01"]]
goProTurnOff[] :=
    execute[goProMakeCommand["bacpac", "PW", "00"]]

(*funkce pro zapnuti a vypnuti spouste (mackani spouste na kamere)*)
goProShutter[] :=
    execute[goProMakeCommand["bacpac", "SH", "01"]]
goProStop[] :=
    execute[goProMakeCommand["bacpac", "SH", "00"]]


(*Camera Mods*)
goProVideoMode[] :=
    execute[goProMakeCommand["camera", "CM", "00"]]
goProPhotoMode[] :=
    execute[goProMakeCommand["camera", "CM", "01"]]
goProBurstMode[] :=
    execute[goProMakeCommand["camera", "CM", "02"]]
goProTimeLapseMode[] :=
    execute[goProMakeCommand["camera", "CM", "03"]]


goProPlayHDMIMode[] :=
    execute[goProMakeCommand["camera", "CM", "05"]](*not usable*)

goProSettingsMode[] :=
    execute[goProMakeCommand["camera", "CM", "07"]](*not usable*)
 

(*Locate*)
goProLocate[] :=
    execute[goProMakeCommand["camera","LL","01"]]
goProLocateStop[] :=
    execute[goProMakeCommand["camera","LL","00"]]

 (*Photo resolutions*)
photoResToCode = <|
   "5M" -> "03",
   "7W" -> "04",
   "12W" -> "05",
   "7M" -> "06"
   |>;
     
   
photoResPossible = {
   "5M",
   "7W",
   "12W",
   "7M"
};

 
goProSetPhotoRes[param_String] :=
    execute[goProMakeCommand["camera","PR",photoResToCode[[param]]]]
goProGetPossiblePhotoRes[] :=
    photoResPossible
 
goProPhotoRes12W[] :=
    goProSetVideoRes[photoResPossible[[3]]]
goProPhotoRes7W[] :=
    goProSetVideoRes[photoResPossible[[2]]]
goProPhotoRes7M[] :=
    goProSetVideoRes[photoResPossible[[4]]]
goProPhotoRes5M[] :=
    goProSetVideoRes[photoResPossible[[1]]]

    
(*video settings*)
videoResToCode = <|
   "WVGA" -> "00",
   "720p" -> "01",
   "960p" -> "02" ,
   "1080p" -> "03",
   "1440p" -> "04",
   "2.7K" -> "05",
   "4K" -> "06",
   "2.7K-17:9" -> "07",
   "4K-17:9" -> "08",
   "1080p-SV" -> "09",
   "720p-SV" -> "0a"
   |>;
   
videoResPossible = {
    "WVGA",
   "720p",
   "960p",
   "1080p",
   "1440p",
   "2.7K",
   "4K",
   "2.7K-17:9",
   "4K-17:9",
   "1080p-SV",
   "720p-SV" 
};

goProSetVideoRes[param_String] :=
    execute[goProMakeCommand["camera","VV",videoResToCode[[param]]]]
goProGetPossibleVideoRes[] :=
    videoResPossible


goProVideoResWVGA[] :=
    goProSetVideoRes[videoResPossible[[1]]]
goProVideoRes720[] :=
    goProSetVideoRes[videoResPossible[[2]]]
goProVideoRes960[] :=
    goProSetVideoRes[videoResPossible[[3]]]
goProVideoRes1080[] :=
    goProSetVideoRes[videoResPossible[[4]]]
goProVideoRes1440[] :=
    goProSetVideoRes[videoResPossible[[5]]]
goProVideoRes2point7K[] :=
    goProSetVideoRes[videoResPossible[[6]]]
goProVideoRes4K[] :=
    goProSetVideoRes[videoResPossible[[7]]]
goProVideoRes2point7K17to9[] :=
    goProSetVideoRes[videoResPossible[[8]]]
goProVideoRes4K17to9[] :=
    goProSetVideoRes[videoResPossible[[9]]]
goProVideoRes1080SV[] :=
    goProSetVideoRes[videoResPossible[[10]]]
goProVideoRes720SV[] :=
    goProSetVideoRes[videoResPossible[[11]]]


(*FPS*)
fpsToCode = <|"12" -> "00",
   "15" -> "01",
   "12p5" -> "0b" ,
   "24" -> "02" ,
   "25" -> "03",
   "30" -> "04",
   "48" -> "05",
   "50" -> "06",
   "60" -> "07",
   "100" -> "08",
   "120"->"09",
   "240" -> "0a"
   |>;
   
fpsPossible = <|
   "00" -> {"240"},
   "01" -> {"60", "120"},
   "0a" -> {"100", "60", "48"},
   "02" -> {"100", "60", "48"},
   "03" -> {"60", "48", "30", "24"},
   "09" -> {"60", "48", "30", "24"},
   "04" -> {"48", "30", "24"},
   "05" -> {"30"},
   "07" -> {"24"},
   "06" -> {"15"},
   "08" -> {"12"}
   |>;
(*to change FPS use this function goProSetFPS[], but you must give fps
 which is possible for actual video resolution, to see which fps is supported use function goProGetPossibleFPS*)
goProSetFPS[f_String] :=
    execute[goProMakeCommand["camera","FS",fpsToCode[[f]]]]


goProGetPossibleFPS[] :=
    fpsPossible[[
    "0" <> StringTake[
    ToString[BaseForm[goProGetStatus["camera", "vv"][[2]], 16]], 1]]]

goProGetPossibleFPS[res_String] :=
    fpsPossible[[videoResToCode[[res]]]]


(*orientatione up/down*)
goProSetOrientationUp[] :=
    execute[goProMakeCommand["camera","UP","00"]]
goProSetOrientationDown[] :=
    execute[goProMakeCommand["camera","UP","01"]]



(*Field of view*)
fovPossible = <|
   "00" -> {"wide"},
   "01" -> {"wide", "narrow"},
   "0a" -> {"wide"},
   "02" -> {"wide"},
   "03" -> {"wide", "medium", "narrow"},
   "09" -> {"wide"},
   "04" -> {"wide"},
   "05" -> {"wide", "medium"},
   "07" -> {"wide", "medium"},
   "06" -> {"wide"},
   "08" -> {"wide"}
   |>;

fovToCode = <|
   "wide" -> "00",
   "medium" -> "01",
   "narrow" -> "02"
   |>;
   
goProSetFOV[f_String] :=
    execute[goProMakeCommand["camera","FV",fovToCode[[f]]]]

goProGetPossibleFOV[] :=
    fovPossible[[
    "0" <> StringTake[
    ToString[BaseForm[goProGetStatus["camera", "vv"][[2]], 16]], 1]]]
   
(*TimeLapse*)
tiToCode = <|
   "0.5" -> "00",
   "1" -> "01",
   "5" -> "05",
   "10"->"0a",
   "30"->"1e",
   "60"->"3c"
   |>;
tiPossible = {"0.5","1","5","10","30","60"}

goProGetPossibleTimeLapse[] :=
    tiPossible
goProSetTimeLapse[f_String] :=
    execute[goProMakeCommand["camera","TI",tiToCode[[f]]]]



(*Burst Rate*)
brToCode = <|
   "3/1"->"00",
   "10/1"->"02",
   "10/2"->"03",
   "30/1"->"04",
   "30/2"->"05",
   "30/3"->"06"
   |>;
brPossible = {"3/1","10/1","10/2","30/1","30/2","30/3"}

goProGetPossibleBurstRate[] :=
    brPossible
goProSetBurstRate[param_String] :=
    execute[goProMakeCommand["camera","BU",brToCode[[param]]]]


(*LoopVideo*)
loToCode = <|
   "off"->"00",
   "5"->"01",
   "20"->"02",
   "60"->"03",
   "max"->"05"
   |>;
loPossible = {"off", "5","20","60","max"}
goProGetPossibleVideoLoop[] :=
    loPossible
goProSetVideoLoop[param_String] :=
    execute[goProMakeCommand["camera","LO",loToCode[[ToLowerCase[param] ]]]]


(*Continuous Shot*)
csToCode = <|
   "1"->"00",
   "3"->"03",
   "5"->"05",
   "10"->"0a"
   |>;
csPossible = {"1", "3","5","10"}
goProGetPossibleContinuousShot[] :=
    csPossible
goProSetContinuousShot[param_String] :=
    execute[goProMakeCommand["camera","CS",csToCode[[ToLowerCase[param] ]]]]

(*Photo in Video*)
pnToCode = <|
    "off"->"00",
    "5"->"01",
    "10"->"02",
    "30"->"03",
    "60"->"04"
|>

pnPossible = {"off","5","10","30","60"}
goProGetPossiblePhotoInVideo[] :=
    pnPossible
goProSetPhotoInVideo[param_String] :=
    execute[goProMakeCommand["camera","PN",pnToCode[[ToLowerCase[param] ]]]]


(*Volume*)
bsToCode = <|
    "off"->"00",
    "70"->"01",
    "100"->"02"
|>
bsPossible = {"off","70","100"}
goProGetPossibleVolume[] :=
    bsPossible
goProSetVolume[param_String] :=
    execute[goProMakeCommand["camera","BS",bsToCode[[ToLowerCase[param] ]]]]


(*Protune*)
ptToCode = <|
    "off"->"00",
    "on"->"01"
|>

ptPossible = {"off","on"}
goProGetPossibleProtune[] :=
    ptPossible
goProSetProtune[param_String] :=
    execute[goProMakeCommand["camera","PT",ptToCode[[ToLowerCase[param] ]]]]
goProSwitchProtuneOn[] :=
    goProSetProtune["on"] 
goProSwitchProtuneOff[] :=
    goProSetProtune["off"] 


(*White Balance - only if protune mode is ON*)
wbToCode = <|
    "auto"->"00",    
    "3000k"->"01",
    "5500k"->"02",
     "6500k"->"03",
    "camraw"->"04",
    "CAMRAW"->"04"
|>

wbPossible = {"auto","3000k","5500k","6500k","CAMRAW","camraw"}
goProGetPossibleWhiteBalance[] :=
    wbPossible
goProSetWhiteBalance[param_String] :=
    execute[goProMakeCommand["camera","WB",wbToCode[[ToLowerCase[param] ]]]]


(*Color Profile (protune)*)
coToCode = <|
    "gopro"->"00",
    "flat"->"01"
|>
coPossible = {"GoPro","Flat"}
goProGetPossibleColorProfile[] :=
    coPossible
goProSetColorProfile[param_String] :=
    execute[goProMakeCommand["camera","CO",coToCode[[ToLowerCase[param] ]]]]

(*ISO*)
gaToCode = <|
    "6400"->"00",
    "1600"->"01",
    "400"->"02"
|>
gaPossible = {"6400","1600","400"}
goProGetPossibleISO[] :=
    gaPossible
goProSetISO[param_String] :=
    execute[goProMakeCommand["camera","GA",gaToCode[[ToLowerCase[param] ]]]]


(*Sharpness*)
spToCode = <|
    "high"->"00",
    "medium"->"01"    ,
    "low"->"02"
|>
spPossible = {"high","medium","low"}
goProGetPossibleSharpness[] :=
    spPossible
goProSetSharpness[param_String] :=
    execute[goProMakeCommand["camera","SP",spToCode[[ToLowerCase[param] ]]]]

(*exposure*)
evToCode = <|
    "-2.0"->"06",
     "-1.5"->"07",
     "-1.0"->"08",
     "-0.5"->"09",
     "0"->"0a",
     "+0.5"->"0b",
     "+1.0"->"0c",
     "+1.5"->"0d",
     "+2.0"->"0e"
|>
evPossible = { "0","-2.0", "-1.5", "-1.0", "-0.5", "+0.5", "+1.0", "+1.5", "+2.0"}
goProGetPossibleExposure[] :=
    evPossible
goProSetExposure[param_String] :=
    execute[goProMakeCommand["camera","EV",evToCode[[ToLowerCase[param] ]]]]


(*Low Light*)
lwToCode = <|
    "off"->"00",
    "on"->"01"
|>
lwPossible = {"off","on"}
goProGetPossibleLowLight[] :=
    lwPossible
goProSetLowLight[param_String] :=
    execute[goProMakeCommand["camera","LW",lwToCode[[ToLowerCase[param] ]]]]
goProSwitchLowLightOn[] :=
    goProSetLowLight["on"]
goProSwitchLowLightOff[] :=
    goProSetLowLight["off"]

(*led*)
lbToCode = <|
    "off"->"00",
    "2"->"01",
    "4"->"02"
|>
lbPossible = {"off","2","4"}
goProGetPossibleLed[] :=
    lbPossible
goProSetLed[param_String] :=
    If[ ToLowerCase[param]=="off",
        execute[goProMakeCommand["camera","LB","00"]],
        execute[goProMakeCommand["camera","LB",lbToCode[[ToLowerCase[param] ]]]]
    ]

(*Spot Meter*)
exToCode = <|
    "off"->"00",
    "on"->"01"
|>
exPossible = {"off","on"}
goProGetPossibleSpotMeter[] :=
    exPossible
goProSetSpotMeter[param_String] :=
    execute[goProMakeCommand["camera","EX",exToCode[[ToLowerCase[param] ]]]]
goProSetSpotMeterOn[] :=
    goProSetSpotMeter["on"]
goProSetSpotMeterOff[] :=
    goProSetSpotMeter["off"]

(*Auto power off*)
aoToCode = <|
    "off"->"00",
    "60"->"01",
    "120"->"02",
    "300"->"03"
|>
aoPossible = {"off","60","120","300"}
goProGetPossibleAutoPowerOff[] :=
    aoPossible
goProSetAutoPowerOff[param_String] :=
    execute[goProMakeCommand["camera","AO",aoToCode[[ToLowerCase[param] ]]]]


(*video mode*)
vmToCode = <|
    "ntsc"->"00",
    "pal"->"01"
|>
vmPossible = {"NTSC","PAL"}
goProGetPossibleVideoMode[] :=
    vmPossible
goProSetVideoMode[param_String] :=
    execute[goProMakeCommand["camera","VM",vmToCode[[ToLowerCase[param] ]]]]
    


(*time Setting*)
toHex[param_String] :=
    (final = "";
     td = param;
     While[StringLength[td] > 0,
      p = StringTake[td, 2];
      td = StringDrop [td, 2];
      (*Print[ToString[BaseForm[ToExpression[p],16]]];*)
      If[ StringLength[ToString[BaseForm[ToExpression[p], 16]]] > 5,
          p = StringTake[ToString[BaseForm[ToExpression[p], 16]], 2],
          p = "0" <> StringTake[ToString[BaseForm[ToExpression[p], 16]], 1]
      ];
      final = final <> "%" <> p;
      ];
     final = StringDrop [final, 1];
     final
    )
  
toFancyTime[param_List] :=
    "20"<>ToString[param[[1]]]<>"-"<>ToString[param[[2]]]<>"-"<>ToString[param[[3]]]<>" "<>ToString[param[[4]]]<>":"<>ToString[param[[5]]]<>":"<>ToString[param[[6]]]

(*<>" "<>ToString[Drop[param,1][[4]]]<>":"<>ToString[Drop[param,1][[5]]]<>":"<>ToString[Drop[param,1][[6]]]*)

goProSetTime[param_String] :=
    If[ StringLength[param] != 12 || !NumberQ[ToExpression[param]],
        Print[StringLength[param]];
        Print["Wrong lenght of input string or it doesn't contains only numbers."],
        execute[goProMakeCommand["camera", "TM", toHex[param]]]
    ]
goProGetTime[] :=
    toFancyTime[Drop[goProGetStatus["camera", "tm"],1]]





(*Settings report*)
(*next command switch keys and values in associative array*)
codeToPhotoRes = Association[photoResToCode[[#]] -> # & /@ Keys[photoResToCode]];
codeToVideoRes = Association[videoResToCode[[#]] -> # & /@ Keys[videoResToCode]];
codeToFPS = Association[fpsToCode[[#]] -> # & /@ Keys[fpsToCode]];
codeToFov = Association[fovToCode[[#]] -> # & /@ Keys[fovToCode]];
codeToBr = Association[brToCode[[#]] -> # & /@ Keys[brToCode]];
codeToLo = Association[loToCode[[#]] -> # & /@ Keys[loToCode]];
codeToCs = Association[csToCode[[#]] -> # & /@ Keys[csToCode]];
codeToPn = Association[pnToCode[[#]] -> # & /@ Keys[pnToCode]];
codeToBs = Association[bsToCode[[#]] -> # & /@ Keys[bsToCode]];
codeToPt = Association[ptToCode[[#]] -> # & /@ Keys[ptToCode]];
codeToWb = Association[wbToCode[[#]] -> # & /@ Keys[wbToCode]];
codeToCo = Association[coToCode[[#]] -> # & /@ Keys[coToCode]];
codeToGa = Association[gaToCode[[#]] -> # & /@ Keys[gaToCode]];
codeToSp = Association[spToCode[[#]] -> # & /@ Keys[spToCode]];
codeToEv = Association[evToCode[[#]] -> # & /@ Keys[evToCode]];
codeToLw = Association[lwToCode[[#]] -> # & /@ Keys[lwToCode]];
codeToLb = Association[lbToCode[[#]] -> # & /@ Keys[lbToCode]];
codeToEx = Association[exToCode[[#]] -> # & /@ Keys[exToCode]];
codeToAo = Association[aoToCode[[#]] -> # & /@ Keys[aoToCode]];
codeToVm = Association[vmToCode[[#]] -> # & /@ Keys[vmToCode]];



codeToMode = <|
    "00"->"Video",
    "01"->"Photo",
    "02"->"Burst",
    "03"->"TimeLapse",
    "07"->"Settings"
|>;




goProGetSettingReport[] :=
    (Print["Time of camera: "<>goProGetTime[]];
     Print[ codeToMode[[ "0"<>ToString[goProGetStatus["camera", "cm"][[2]] ] ]] <> " mode" ];
     Print["Video Resolution: " <> 
         codeToVideoRes[[
             "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "vv"][[2]], 16]], 1]
         ]] <>" "<>
         codeToFov[[ "0" <> ToString[goProGetStatus["camera", "fv"][[2]]] ]]
     ];
     Print["Photo Resolution: " <> codeToPhotoRes[[ "0"<> ToString[goProGetStatus["camera", "pr"][[2]] ] ]] ];
     Print["FPS: "<>
         codeToFPS[[
             "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "fs"][[2]], 16]], 1]
         ]]
     ];
     pom = goProGetStatus["camera", "ti"][[2]];
     If[ pom==0,
         pom = 0.5
     ];
     Print["TimeLapse Interval: "<> ToString[pom] <> "s" ];
     Print["Burst Rate: " <> codeToBr[["0" <> ToString[ goProGetStatus["camera", "bu"][[2]] ] ]] <> "s" ];
     Print["Video Loop: " <> codeToLo[["0"<> ToString[goProGetStatus["camera", "lo"][[2]] ] ]] ];
     Print["Continuous Shot: "<>
         codeToCs[[
             "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "cs"][[2]], 16]], 1]
         ]]
     ];
     Print["Photo in Video interval : " <> codeToPn[["0"<> ToString[goProGetStatus["camera", "pn"][[2]] ] ]] ];
     Print["Volume : " <> codeToBs[["0"<> ToString[goProGetStatus["camera", "bs"][[2]] ] ]] ];
     Print["Led : " <> codeToLb[["0"<> ToString[goProGetStatus["camera", "lb"][[2]] ] ]] ];
     Print["Low Light : " <> codeToLw[["0"<> ToString[goProGetStatus["camera", "lw"][[2]] ] ]] ];
     Print["Spot meter : " <> codeToEx[["0"<> ToString[goProGetStatus["camera", "ex"][[2]] ] ]] ];
     Print["Auto power off : " <> codeToAo[["0"<> ToString[goProGetStatus["camera", "ao"][[2]] ] ]] ];
     Print["Video Mode : " <> codeToVm[["0"<> ToString[goProGetStatus["camera", "vm"][[2]] ] ]] ];
     Print["PROTUNE OPTIONS: "];
     Print["Protune : " <> codeToPt[["0"<> ToString[goProGetStatus["camera", "pt"][[2]] ] ]] ];
     Print["White Balance : " <> codeToWb[["0"<> ToString[goProGetStatus["camera", "wb"][[2]] ] ]] ];
     Print["Color Profile : " <> codeToCo[["0"<> ToString[goProGetStatus["camera", "co"][[2]] ] ]] ];
     Print["ISO : " <> codeToGa[["0"<> ToString[goProGetStatus["camera", "ga"][[2]] ] ]] ];
     Print["Sharpness : " <> codeToSp[["0"<> ToString[goProGetStatus["camera", "sp"][[2]] ] ]] ];
     Print["Exposure compensation : " <> codeToEv[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "ev"][[2]], 16]], 1] ]] ];
    
    )





(*saving settings*)

(* {mode,VideoRes,Fov,FPS, PhotoRes, Time-Lapse-Interval}  *)
(*returns code of time interval which is set*)    
getTimeInterval[] :=
    (
    pom = goProGetStatus["camera", "ti"][[2]];
    If[ pom==0,
        pom = 0.5
    ];
    ToString[pom]
    )


(*download settings of camera and transform them to list*)
settingsToList[] :=
    {codeToMode[[ "0"<>ToString[goProGetStatus["camera", "cm"][[2]] ] ]],
    codeToVideoRes[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "vv"][[2]], 16]], 1] ]],
    codeToFov[[ "0" <> ToString[goProGetStatus["camera", "fv"][[2]]] ]],
    codeToFPS[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "fs"][[2]], 16]], 1] ]],
    codeToPhotoRes[["0" <> ToString[goProGetStatus["camera", "pr"][[2]] ] ]],
    getTimeInterval[];
    codeToBr[["0" <> ToString[goProGetStatus["camera", "bu"][[2]] ] ]],
    codeToLo[["0" <> ToString[goProGetStatus["camera", "lo"][[2]] ] ]],
    codeToCs[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "cs"][[2]], 16]], 1] ]],
    codeToPn[["0" <> ToString[goProGetStatus["camera", "pn"][[2]] ] ]],
    codeToBs[["0" <> ToString[goProGetStatus["camera", "bs"][[2]] ] ]],
    codeToLb[["0" <> ToString[goProGetStatus["camera", "lb"][[2]] ] ]],
    codeToLw[["0" <> ToString[goProGetStatus["camera", "lw"][[2]] ] ]],
    codeToEx[["0" <> ToString[goProGetStatus["camera", "ex"][[2]] ] ]],
    codeToAo[["0" <> ToString[goProGetStatus["camera", "ao"][[2]] ] ]];
    codeToVm[["0" <> ToString[goProGetStatus["camera", "vm"][[2]] ] ]];
    codeToPt[["0" <> ToString[goProGetStatus["camera", "pt"][[2]] ] ]],
    codeToWb[["0" <> ToString[goProGetStatus["camera", "wb"][[2]] ] ]],
    codeToCo[["0" <> ToString[goProGetStatus["camera", "co"][[2]] ] ]],
    codeToGa[["0" <> ToString[goProGetStatus["camera", "ga"][[2]] ] ]],
    codeToSp[["0" <> ToString[goProGetStatus["camera", "sp"][[2]] ] ]],
    codeToEv[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "ev"][[2]], 16]], 1] ]]
    
    }

(*exports settings to name specified by parameter*)
goProSaveSettings[file_String] :=
    Export[file,settingsToList[]]


(*downloading content*)


(*SETTINGS via Options*)

(*Options[goProSetVideo] = {videoResolution -> videoResolutionDef,
	fps -> fpsDef,
	fov -> fovDef,
	videoMode->videoModeDef};
goProSetVideo[OptionsPattern[]] := (
	downloadSettings[];
   
    goProSetVideoRes[OptionValue[{videoResolution}][[1]]];
    If[ MemberQ[goProGetPossibleFPS[OptionValue[videoResolution]], 
      OptionValue[{fps}][[1]]],
        goProSetFPS[OptionValue[{fps}][[1]]],
        s = "The FPS value (" <> OptionValue[{fps}][[1]] <> 
          ") is not possible for this video resolution ("  <> 
          OptionValue[{videoResolution}][[1]] <> 
          "), FPS was set to the highest value possible (" <> 
          goProGetPossibleFPS[OptionValue[videoResolution]][[1]] <> 
          "). If you wish to see which FPS can be used for certain video \
resolution call goProGetPossibleFPS[ \"" <> 
          OptionValue[videoResolution] <> "\" ]";
        MessageDialog[s];
    ];
    goProSetFOV[OptionValue[{fov}][[1]]];
    goProSetVideoMode[OptionValue[{videoMode}][[1]]]
    
    )*)
    
   
 
 
downloadSettings[]:=(
	
    videoResolutionDef=codeToVideoRes[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "vv"][[2]], 16]], 1] ]];
    fovDef=codeToFov[[ "0" <> ToString[goProGetStatus["camera", "fv"][[2]]] ]];
    fpsDef=codeToFPS[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "fs"][[2]], 16]], 1] ]];
    photoResolutionDef=codeToPhotoRes[["0" <> ToString[goProGetStatus["camera", "pr"][[2]] ] ]];
    timeLapseDef=getTimeInterval[];
    burstRateDef=codeToBr[["0" <> ToString[goProGetStatus["camera", "bu"][[2]] ] ]];
    loopVideoDef=codeToLo[["0" <> ToString[goProGetStatus["camera", "lo"][[2]] ] ]];
    continuousShotDef=codeToCs[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "cs"][[2]], 16]], 1] ]];
    photoInVideoDef=codeToPn[["0" <> ToString[goProGetStatus["camera", "pn"][[2]] ] ]];
    volumeDef=codeToBs[["0" <> ToString[goProGetStatus["camera", "bs"][[2]] ] ]];
    ledDef=codeToLb[["0" <> ToString[goProGetStatus["camera", "lb"][[2]] ] ]];
    lowLightDef=codeToLw[["0" <> ToString[goProGetStatus["camera", "lw"][[2]] ] ]];
    spotMeterDef=codeToEx[["0" <> ToString[goProGetStatus["camera", "ex"][[2]] ] ]];
    autoPowerOffDef=codeToAo[["0" <> ToString[goProGetStatus["camera", "ao"][[2]] ] ]];
    videoModeDef=codeToVm[["0" <> ToString[goProGetStatus["camera", "vm"][[2]] ] ]];
    protuneDef=codeToPt[["0" <> ToString[goProGetStatus["camera", "pt"][[2]] ] ]];
    whiteBalanceDef=codeToWb[["0" <> ToString[goProGetStatus["camera", "wb"][[2]] ] ]];
    colorProfileDef=codeToCo[["0" <> ToString[goProGetStatus["camera", "co"][[2]] ] ]];
    isoDef=codeToGa[["0" <> ToString[goProGetStatus["camera", "ga"][[2]] ] ]];
    sharpnessDef=codeToSp[["0" <> ToString[goProGetStatus["camera", "sp"][[2]] ] ]];
    exposureDef=codeToEv[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "ev"][[2]], 16]], 1] ]]
    )
(*Default options for parameters*)
(*videoResolutionDef = "1080p";
fpsDef= "30";
fovDef = "wide";
videoModeDef="NTSC";
photoResolutionDef="12W";
loopVideoDef="off";
burstRateDef="30/1";
timeLapseDef="1";
continuousShotDef="1";
photoInVideoDef="off";
volumeDef="off";
ledDef="4";
lowLightDef="off";
spotMeterDef="off";
autoPowerOffDef="off";
protuneDef="off";
whiteBalanceDef="auto";
colorProfileDef="GoPro";
isoDef=goProGetPossibleISO[][[1]];
sharpnessDef=goProGetPossibleSharpness[][[1]];
exposureDef=goProGetPossibleExposure[][[1]];*)

(*Options[goProSet] = {videoResolution -> "1080p",
	fps -> "30",
	fov -> "wide", 
	videoMode->"NTSC",
    photoResolution->"12W",
    loopVideo->"off",
    burstRate->"30/1",
    timeLapse->"1",
    continuousShot->"1",
    photoInVideo->"off",
    volume->"off", 
    led->"4", 
    lowLight->"off",
    spotMeter->"off", 
    autoPowerOff->"off",
    protune->"off",
    whiteBalance->"auto",
    colorProfile->"GoPro",
    iso->goProGetPossibleISO[][[1]],
    sharpness->goProGetPossibleSharpness[][[1]],
    exposure->goProGetPossibleExposure[][[1]]}*)

 
Options[goProSet] = {videoResolution -> videoResolutionDef,
	fps -> fpsDef,
	fov -> fovDef, 
	videoMode->videoModeDef,
    photoResolution->photoResolutionDef,
    loopVideo->loopVideoDef,
    burstRate->burstRateDef,
    timeLapse->timeLapseDef,
    continuousShot->continuousShotDef,
    photoInVideo->photoInVideoDef,
    volume->volumeDef, 
    led->ledDef, 
    lowLight->lowLightDef,
    spotMeter->spotMeterDef, 
    autoPowerOff->autoPowerOffDef,
    protune->protuneDef,
    whiteBalance->whiteBalanceDef,
    colorProfile->colorProfileDef,
    iso->iso,
    sharpness->sharpnessDef,
    exposure->exposureDef}

goProSet[OptionsPattern[]] :=(downloadSettings[];
    (*goProSetVideo[Flatten[{videoResolution->OptionValue[videoResolution],fps-> OptionValue[fps], fov->OptionValue[fov],videoMode->OptionValue[videoMode]}]];*)
    
	 (*I am using the test on SameQ so i won't do request which will set the same setting as which is already set*)	
	goProSetVideoRes[OptionValue[videoResolution]];
	goProSetFPS[OptionValue[fps]];
	
    (*If[ MemberQ[goProGetPossibleFPS[OptionValue[videoResolution]], OptionValue[fps] ],
        
    ];*)
    If[!SameQ[OptionValue[fov],fovDef],goProSetFOV[OptionValue[fov]]];
    If[!SameQ[OptionValue[videoMode],videoModeDef],goProSetVideoMode[OptionValue[videoMode]]];
	 
	 
    
     If[!SameQ[OptionValue[photoResolution],photoResolutionDef],goProSetPhotoRes[OptionValue[photoResolution]]];   
     If[!SameQ[OptionValue[loopVideo],loopVideoDef],goProSetVideoLoop[OptionValue[loopVideo]]];
     If[!SameQ[OptionValue[burstRate],burstRateDef],goProSetBurstRate[OptionValue[burstRate]]];
     If[!SameQ[OptionValue[timeLapse],timeLapseDef],goProSetTimeLapse[OptionValue[timeLapse]]];
     If[!SameQ[OptionValue[continuousShot],continuousShotDef],goProSetContinuousShot[OptionValue[continuousShot]]];
     If[!SameQ[OptionValue[photoInVideo],photoInVideoDef],goProSetPhotoInVideo[OptionValue[photoInVideo]]];
     If[!SameQ[OptionValue[volume],volumeDef],goProSetVolume[OptionValue[volume]]];
     If[!SameQ[OptionValue[led],ledDef],goProSetLed[OptionValue[led]]];
     If[!SameQ[OptionValue[lowLight],lowLightDef],goProSetLowLight[OptionValue[lowLight]]];
     If[!SameQ[OptionValue[spotMeter],spotMeterDef],goProSetSpotMeter[OptionValue[spotMeter]]];
     If[!SameQ[OptionValue[autoPowerOff],autoPowerOffDef],goProSetAutoPowerOff[OptionValue[autoPowerOff]]];
     If[!SameQ[OptionValue[protune],protuneDef],goProSetProtune[OptionValue[protune]]];
     If[!SameQ[OptionValue[whiteBalance],whiteBalanceDef],goProSetWhiteBalance[OptionValue[whiteBalance]]];
     If[!SameQ[OptionValue[colorProfile],colorProfileDef],goProSetColorProfile[OptionValue[colorProfile]]];
     If[!SameQ[OptionValue[iso],isoDef],goProSetISO[OptionValue[iso]]];
     If[!SameQ[OptionValue[sharpness],sharpnessDef],goProSetSharpness[OptionValue[sharpness]]];
     If[!SameQ[OptionValue[exposure],exposureDef],goProSetExposure[OptionValue[exposure]]];
    )
  


End[]

EndPackage[]