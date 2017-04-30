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

goProSetPassword::usage =
    "goProSetPassword[p_String] user provides password for usage of goProController package"

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



(*video resolutions*)
goProSetVideoRes::usage =
    "goProSetVideoRes[_String] Sets Video resolution acording to parameter. To see which parameter you can be used use function goProGetPossibleVideoRes[]"
goProGetPossibleVideoRes::usage =
    "goProGetPossibleVideoRes[ ] Returns list of possible parameters for function goProSetVideoRes[_String]"

(*goProVideoRes4K::usage =
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
    *)


(*FPS*)
goProGetPossibleFPS::usage =
    "goProGetPossibleFPS[ ] Returns list of possible FPS for actual video resolution"
goProGetPossibleFPS::usage =
    "goProGetPossibleFPS[ _String] Returns list of possible FPS for video resolution specified by parameter"
goProSetFPS::usage =
    "goProSetFPS[_String] Sets FPS to given parameter, but only if this amount of FPS is supported by actual video resolution.
	 To see which parameter you can use for actual video resolution use goProGetPossibleFPS function"


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


(*Modes*)
goProMode::usage=
	"goProMode[ _String] Switches mode to parameter given. To see which parameter you can use call goProGetPossibleModes[]."
goProGetPossibleModes::usage=
	"goProGetPossibleModes[ ] returns possible camera modes ."
goProSetBootMode::usage=
	"goProSetBootMode[ _String] Sets boot mode to parameter given, to see options possible call goProGetPossibleBootMode[]. "
goProGetPossibleBootMode::usage=
	"goProGetPossibleBootMode[] returns possible options for boot mode. "




(*saving settings to file*)
goProSaveSettings::usage =
    "goProSaveSettings[_String] saves settings to the file in parameter."





goProGetFileList::usage=
	"goProGetFileList[] returns list of files which were aquired by the camera." 
goProDownloadFile::usage=
	"goProDownloadFile[name_String,destination_String] download file which name was given as first parameter to the destination specified in second parameter.
	 goProDownloadFile[list_List,destination_String] download files in list from first parameter to the destination specified in second parameter. "
	 
	
goProDownloadAllFiles::usage=
	"goProDownloadAllFiles[ _String] downloads all files from camera to destination given."
goProGetFileURL::usage=
	"goProGetFileURL[_String] returns URL string of file which name was given in parameter."	
	
goProSetURLBase::usage=
	"goProSetURLBase[_String] lets you set urlBase variable. Use it if you want to set another folder on camera for downloading data from camera."

goProGetURLBase::usage=
	"goProGetURLBase[] returns value of urlBase variable, which stores url for folder from which you can download data from camera."
	
	
	
goProDeleteAll::usage=
	"goProDeleteAll[ ] deletes all files on camera."
goProDeleteLastFile::usage=
	"goProDeleteLastFile[ ] deletes last file captured on camera."
 (*goProDeleteFile::usage=
	"goProDeleteFile[_String] deletes file on camera which name you put as parameter."*)





(*SETTINGS*)
goProSet::usage =
    "goProSet[ ] enables to set camera via rules such as: {fps,fov,videoResolution,photoResolution,...}"

goProGetSettingReportAssociation::usage=
	"goProGetSettingReportAssociation[ ] returns association field with all settings of camera."

goProGetVariables::usage=
	"goProGetVariables[ ], returns all variables for goProSet function."

goProGet::usage=
	"goProGet[_String] returns value set to parameter given such ase {videoResolution->1080p}"

(*goProSetVideo::usage =
    "goProSetVideo[ ] enables to set video settings such as fps, fov, videoResolution through rules, for example: goProSetVideo[{fps->\"30\",fov->->\"wide\", videoResolution->\"1080p\" }]. All parameters are optional."
*)


goProTextModeOn::usage=
	"goProTextModeOn[] functions goProSet* will only return String containing url adress."
goProTextModeOff::usage=
	"goProTextModeOff[] functions goProSet* will send requests on url adress of settings."
	
	

goProGetCameraModelPossible::usage=
	"goProGetCameraModelPossible[] returns possible parameters for goProSetCameraModel. "
goProSetCameraModel::usage=
	"goProSetCameraModel[ _String] Lets you specify model for camera which you set by goProSetCamera. It is 
	important to know camera model for few functions of this library."

(* ::Section:: *)
(* Private Definitions *)


Begin["`Private`"]


Print["camera: "<>camera]
goProGetCamera[]:=camera

model="";



modelHero3Plus={"Silver","Black"}
modelHero3={"White","Silver","Black"}

goProSetCameraModel::model="First you have to specificate camera model! Call goProSetCameraModel[_String]";
goProSetCameraModel::wrong="`1` - This parameter is not supported for camera model. See goProGetCameraModelPossible.";
goProSetCameraModel::noNeed="You don't have to set camera model for `1`";
goProSetCameraModel[param_String]:=Switch[camera,
	"HERO3+",If[MemberQ[modelHero3Plus,param],model=param,
		Message[goProSetCameraModel::wrong,param]
	],
	"HERO3",If[MemberQ[modelHero3,param],model=param,
		Message[goProSetCameraModel::wrong,param]
	],
	_,Message[goProSetCameraModel::noNeed,camera]	
]
goProGetCameraModelPossible[]:=Switch[camera,
	"HERO3+",modelHero3Plus,
	"HERO3",modelHero3,
	_,Message[goProSetCameraModel::noNeed,camera]
]
goProGetModel[] := model
goProPassword="";



(*adresa kamery na siti, kterou kamera vysila*)
goProUrl = "http://10.5.5.9/";


(*Funkce pro slozeni url pro ovladani kamery, unit (bacpac||camera), command (prikat), parram (parametr prikazu)*)
goProMakeCommand[unit_String, command_String, parram_String] :=
    goProUrl <> unit <> "/" <> command <> "?t=" <> goProPassword <>
     "&p=%" <> parram
     
 goProMakeCommand[unit_String, command_String] :=
    goProUrl <> unit <> "/" <> command <> "?t=" <> goProPassword



textMode=0;
goProTextModeOn[]:=(textMode=1;)
goProTextModeOff[]:=(textMode=0;)

goProSetPassword::goProPassword="First you have to set password for Wifi! Call goProSetPassword[_String]";

(*spusteni prikazu exec pomoci HTTPRequest a URLRead*)
execute[exec_String] :=(If[goProPassword=="",Message[goProSetPassword::goProPassword, goProPassword],
	If[textMode==0,
		request = HTTPRequest[exec];
    	URLRead[request],
    	exec
    ]
])

goProGetStatus[unit_String, param_String] :=If[goProPassword=="",Message[goProSetPassword::goProPassword, goProPassword],
    URLExecute[HTTPRequest[goProUrl <> unit <> "/" <> param <> "?t=" <> goProPassword]]
]

(*Funkce pro inicializaci hesla*)
goProSetPassword::timeOut="Camera not found."
goProSetPassword[password_String] := (goProPassword = password;If[SameQ[PingTime["10.5.5.9"], $TimedOut], Message[goProSetPassword::timeOut], downloadSettings[];])

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


modesToCode=<|
	"Video"->"00",
	"Photo"->"01",
	"Burst"->"02",
	"TimeLapse"->"03"
|>

goProGetPossibleModes[]:=Keys[modesToCode]
goProMode[param_String]:=execute[goProMakeCommand["camera", "CM", modesToCode[[param]] ]]




goProSetBootMode[param_String]:=execute[goProMakeCommand["camera", "DM", modesToCode[[param]] ]]
goProGetPossibleBootMode[]:=Keys[modesToCode]


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
   "7M" -> "06",
   "11W"->"00",
   "8M"->"01",
   "5W"->"02"
   |>;


photoResPossible = {
   "5M",
   "7W",
   "12W",
   "7M"
};
photoResPossibleHERO2 = {
   "5M",
   "8M",
   "11W",
   "5W"
};

goProSet::param="Unsupported parameter `1`. Maybe you have set the wrong camera model."
goProSetPhotoRes[param_String] :=Switch[camera,
	"HERO3+",
		execute[goProMakeCommand["camera","PR",photoResToCode[[param]]]],
	"HERO2",
		execute[goProMakeCommand["camera","PR",photoResToCode[[param]]]],
	"HERO3",Switch[model,
		"Black",
			execute[goProMakeCommand["camera","PR",photoResToCode[[param]]]],
		"Silver",
			execute[goProMakeCommand["camera","PR",photoResToCode[[param]]]],
		"White",
			execute[goProMakeCommand["camera","PR",photoResToCode[[param]]]],
		_,Message[goProSetCameraModel::model]		
	]
]
    
    
goProGetPossiblePhotoRes[] :=Switch[camera,
	"HERO3+",photoResPossible,
	"HERO2",photoResPossibleHERO2,
	"HERO3",Switch[model,
		"Black",photoResPossible,
		"Silver",photoResPossibleHERO2
		,
		"White",photoResPossibleHERO2
		,
		_,Message[goProSetCameraModel::model]
		]	
	]


photoResPossibleHERO2

    photoResPossible




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

videoResToCodeHERO2=<|
   "WVGA-60" -> "00",
   "WVGA-120" -> "01",
   "720p-30" -> "02" ,
   "720p-60" -> "03",
   "960p-30" -> "04",
   "960p-48" -> "05",
   "1080p-30" -> "06"
|>
videoResToCodeHERO3=<|
	"WVGA-240" -> "00",
   "720p-120" -> "01",
   "960p-100" -> "02" ,
   "1080p-60" -> "03",
   "1440p-40" -> "04",
   "2.7K-30" -> "05",
   "4K" -> "06",
   "2.7K-17:9" -> "07",
   "4K-17:9" -> "08"
   |>

videoResPossibleHERO3Plus = Keys[videoResToCode];
videoResPossibleHERO2=Keys[videoResToCodeHERO2];
videoResPossibleHERO3=Keys[videoResToCodeHERO3];

goProSetVideoRes[param_String] :=Switch[camera, "HERO3+",execute[goProMakeCommand["camera","VV",videoResToCode[[param]]]],
	"HERO2",execute[goProMakeCommand["camera","VR",videoResToCodeHERO2[[param]]]],
	"HERO3",(Switch[model,
		"Black",execute[goProMakeCommand["camera","VV",videoResToCodeHERO3[[param]]]],
		"Silver",execute[goProMakeCommand["camera","VR",videoResToCodeHERO2[[param]]]],
		"White",execute[goProMakeCommand["camera","VR",videoResToCodeHERO2[[param]]]],			
		_,Message[goProSetCameraModel::model,model]
		])
	]
	
goProGetPossibleVideoRes[] :=Switch[camera, "HERO3+",videoResPossibleHERO3Plus,
	"HERO2",videoResPossibleHERO2,
	"HERO3",(Switch[model,
		"Black",videoResPossibleHERO3,
		"Silver",videoResPossibleHERO2,	
		"White",videoResPossibleHERO2,
		_,Message[goProSetCameraModel::model,model]
		])
	]




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
   
fpsPossibleAll={"240","120","100","60","50", "48", "30","25", "24","12p5","15"}
   
(*to change FPS use this function goProSetFPS[], but you must give fps
 which is possible for actual video resolution, to see which fps is supported use function goProGetPossibleFPS*)
goProSetFPS::notSupported="This function is not supported for this camera (`1`)."
goProSetFPS[f_String] := execute[goProMakeCommand["camera","FS",fpsToCode[[f]]]]


goProGetPossibleFPS[] :=Switch[camera,"HERO3+",
    fpsPossible[[
    "0" <> StringTake[
    ToString[BaseForm[goProGetStatus["camera", "vv"][[2]], 16]], 1]]],
    _,fpsPossibleAll
]
    

goProGetPossibleFPS[res_String] :=Switch[camera,"HERO3+",
    fpsPossible[[videoResToCode[[res]]]],
    _,Message[goProSetFPS::notSupported,camera]
]
    


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

goProSetBurstRate::notSupported="This camera model(`1`) can't set Burst rate. It only supports 10/1s burst mode."
goProGetPossibleBurstRate[] := Switch[camera,"HERO3+", brPossible,
	"HERO3", brPossible,
	"HERO2",Message[goProSetBurstRate::notSupported,camera]
]
	
goProSetBurstRate[param_String] :=Switch[camera,"HERO3+", execute[goProMakeCommand["camera","BU",brToCode[[param]]]],
	"HERO3", execute[goProMakeCommand["camera","BU",brToCode[[param]]]],
	"HERO2",Message[goProSetBurstRate::notSupported,camera]
]


(*LoopVideo*)
loToCode = <|
   "off"->"00",
   "5"->"01",
   "20"->"02",
   "60"->"03",
   "max"->"05"
   |>;
loPossible = {"off", "5","20","60","max"}

goProSetVideoLoop::notSupported="This camera model(`1`) can't use looping video."
goProGetPossibleVideoLoop[] :=Switch[camera,"HERO3+", loPossible,
	"HERO3", loPossible,
	"HERO2",Message[goProSetVideoLoop::notSupported,camera]
]
goProSetVideoLoop[param_String] :=Switch[camera,"HERO3+",  execute[goProMakeCommand["camera","LO",loToCode[[ToLowerCase[param] ]]]],
	"HERO3",  execute[goProMakeCommand["camera","LO",loToCode[[ToLowerCase[param] ]]]],
	"HERO2",Message[goProSetVideoLoop::notSupported,camera]
]
   


(*Continuous Shot*)
csToCode = <|
   "1"->"00",
   "3"->"03",
   "5"->"05",
   "10"->"0a"
   |>;
   
goProSetContinuousShot::notSupported ="This camera model(`1`) can't use continuous shot."
csPossible = {"1", "3","5","10"}
goProGetPossibleContinuousShot[] := Switch[camera,"HERO3+", csPossible,
	"HERO3", csPossible,
	"HERO2",Message[goProSetContinuousShot::notSupported,camera]
]
    csPossible
goProSetContinuousShot[param_String] :=Switch[camera,"HERO3+", execute[goProMakeCommand["camera","CS",csToCode[[ToLowerCase[param] ]]]],
	"HERO3", execute[goProMakeCommand["camera","CS",csToCode[[ToLowerCase[param] ]]]],
	"HERO2",Message[goProSetContinuousShot::notSupported,camera]
]
    

(*Photo in Video*)
pnToCode = <|
    "off"->"00",
    "5"->"01",
    "10"->"02",
    "30"->"03",
    "60"->"04"
|>

pnPossible = {"off","5","10","30","60"}

goProSetPhotoInVideo::notSupported ="This camera model(`1`) can't use photo in video."

goProGetPossiblePhotoInVideo[] :=Switch[camera,"HERO3+",pnPossible,
	"HERO3",pnPossible,
	"HERO2",Message[goProSetPhotoInVideo::notSupported,camera]
]
    
goProSetPhotoInVideo[param_String] :=Switch[camera,"HERO3+", execute[goProMakeCommand["camera","PN",pnToCode[[ToLowerCase[param] ]]]],
	"HERO3", execute[goProMakeCommand["camera","PN",pnToCode[[ToLowerCase[param] ]]]],
	"HERO2",Message[goProSetPhotoInVideo::notSupported,camera]
]

   


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

goProSetWhiteBalance::notSupported ="This camera model(`1`) can't use white balance."

wbPossible = {"auto","3000k","5500k","6500k","CAMRAW","camraw"}

goProGetPossibleWhiteBalance[] :=Switch[camera,"HERO3+", wbPossible,
	"HERO3", wbPossible,
	"HERO2",Message[goProSetWhiteBalance::notSupported,camera]
]
    
goProSetWhiteBalance[param_String] := Switch[camera,"HERO3+",  execute[goProMakeCommand["camera","WB",wbToCode[[ToLowerCase[param] ]]]],
	"HERO3",  execute[goProMakeCommand["camera","WB",wbToCode[[ToLowerCase[param] ]]]],
	"HERO2",Message[goProSetWhiteBalance::notSupported,camera]
]
   


(*Color Profile (protune)*)
coToCode = <|
    "gopro"->"00",
    "flat"->"01"
|>
coPossible = {"GoPro","Flat"}


goProSetColorProfile::notSupported ="This camera model(`1`) can't change color profile."

goProGetPossibleColorProfile[] :=Switch[camera,"HERO3+",  coPossible,
	"HERO3",  Message[goProSetColorProfile::notSupported,camera],
	"HERO2",Message[goProSetColorProfile::notSupported,camera]
]
    
goProSetColorProfile[param_String] :=Switch[camera,"HERO3+",   execute[goProMakeCommand["camera","CO",coToCode[[ToLowerCase[param] ]]]],
	"HERO3", Message[goProSetColorProfile::notSupported,camera],
	"HERO2",Message[goProSetColorProfile::notSupported,camera]
]
   

(*ISO*)
gaToCode = <|
    "6400"->"00",
    "1600"->"01",
    "400"->"02"
|>
gaPossible = {"6400","1600","400"}

goProSetISO::notSupported ="This camera model(`1`) can't change ISO."

goProGetPossibleISO[] :=Switch[camera,"HERO3+",  gaPossible,
	"HERO3",  Message[goProSetISO::notSupported,camera],
	"HERO2",Message[goProSetISO::notSupported,camera]
]
goProSetISO[param_String] :=Switch[camera,"HERO3+", execute[goProMakeCommand["camera","GA",gaToCode[[ToLowerCase[param] ]]]],
	"HERO3", Message[goProSetISO::notSupported,camera],	
	"HERO2",Message[goProSetISO::notSupported,camera]
]
   


(*Sharpness*)
spToCode = <|
    "high"->"00",
    "medium"->"01"    ,
    "low"->"02"
|>
spPossible = {"high","medium","low"}

goProSetSharpness::notSupported ="This camera model(`1`) can't change sharpness."

goProGetPossibleSharpness[] :=Switch[camera,"HERO3+",spPossible,
	"HERO3",Message[goProSetISO::notSupported,camera],
	"HERO2",Message[goProSetSharpness::notSupported,camera]
]
   
goProSetSharpness[param_String] :=Switch[camera,"HERO3+",   execute[goProMakeCommand["camera","SP",spToCode[[ToLowerCase[param] ]]]],
	"HERO3",  Message[goProSetISO::notSupported,camera],
	"HERO2",Message[goProSetSharpness::notSupported,camera]
]

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

goProSetExposure::notSupported ="This camera model(`1`) can't change exposure."
goProGetPossibleExposure[] :=Switch[camera,"HERO3+",evPossible,
	"HERO3",Message[goProSetISO::notSupported,camera],
	"HERO2",Message[goProSetExposure::notSupported,camera]
]
    
goProSetExposure[param_String] :=Switch[camera,"HERO3+",execute[goProMakeCommand["camera","EV",evToCode[[ToLowerCase[param] ]]]],
	"HERO3",Message[goProSetISO::notSupported,camera],
	"HERO2",Message[goProSetExposure::notSupported,camera]
]
    


(*Low Light*)
lwToCode = <|
    "off"->"00",
    "on"->"01"
|>
lwPossible = {"off","on"}

goProSetLowLight::notSupported ="This camera model(`1`) can't low light setting."

goProGetPossibleLowLight[] :=Switch[camera,"HERO3+",lwPossible,
	"HERO3",lwPossible,
	"HERO2",Message[goProSetLowLight::notSupported,camera]
]
    
goProSetLowLight[param_String] :=
Switch[camera,"HERO3+", execute[goProMakeCommand["camera","LW",lwToCode[[ToLowerCase[param] ]]]],
	"HERO3", execute[goProMakeCommand["camera","LW",lwToCode[[ToLowerCase[param] ]]]],
	"HERO2",Message[goProSetLowLight::notSupported,camera]
]
   
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




(* ::Subsection:: *)
(* Setting all parameters in one function *)

(*function downloadSettings[] will download all possible settings from camera and save them to the
	vareiables with suffix Def (Default), this function will be called before every usage of function goProSet[].
*)


    modeDef=""
    videoResolutionDef=""
    fovDef=""
    fpsDef=""
    photoResolutionDef=""
    burstRateDef=""
    timeLapseDef=""
    loopVideoDef=""
    continuousShotDef=""
    photoInVideoDef=""
    volumeDef=""
    ledDef=""
    lowLightDef=""
    spotMeterDef=""
    autoPowerOffDef=""
    videoModeDef=""
    protuneDef=""
    whiteBalanceDef=""
    colorProfileDef=""
    isoDef=""
    sharpnessDef=""
    exposureDef=""
    bootModeDef=""


downloadSettings[]:=(
	Switch[camera,"HERO3+",(
	modeDef=codeToMode[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "cm"][[2]], 16]], 1] ]];
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
    exposureDef=codeToEv[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "ev"][[2]], 16]], 1] ]];
    bootModeDef=codeToMode[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "dm"][[2]], 16]], 1] ]];
    ),
    "HERO3",(
	modeDef=codeToMode[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "cm"][[2]], 16]], 1] ]];
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
    bootModeDef=codeToMode[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "dm"][[2]], 16]], 1] ]];
    ),
    "HERO2",(
    modeDef=codeToMode[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "cm"][[2]], 16]], 1] ]];
    videoResolutionDef=codeToVideoRes[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "vv"][[2]], 16]], 1] ]];
    fovDef=codeToFov[[ "0" <> ToString[goProGetStatus["camera", "fv"][[2]]] ]];
    fpsDef=codeToFPS[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "fs"][[2]], 16]], 1] ]];
    photoResolutionDef=codeToPhotoRes[["0" <> ToString[goProGetStatus["camera", "pr"][[2]] ] ]];
    timeLapseDef=getTimeInterval[];
    volumeDef=codeToBs[["0" <> ToString[goProGetStatus["camera", "bs"][[2]] ] ]];
    ledDef=codeToLb[["0" <> ToString[goProGetStatus["camera", "lb"][[2]] ] ]];
    spotMeterDef=codeToEx[["0" <> ToString[goProGetStatus["camera", "ex"][[2]] ] ]];
    autoPowerOffDef=codeToAo[["0" <> ToString[goProGetStatus["camera", "ao"][[2]] ] ]];
    videoModeDef=codeToVm[["0" <> ToString[goProGetStatus["camera", "vm"][[2]] ] ]];
    protuneDef=codeToPt[["0" <> ToString[goProGetStatus["camera", "pt"][[2]] ] ]];
    bootModeDef=codeToMode[[ "0" <> StringTake[ToString[BaseForm[goProGetStatus["camera", "dm"][[2]], 16]], 1] ]];
    )
	]
    
    )





Options[goProSet] = Switch[camera,"HERO3+",{
	videoResolution -> videoResolutionDef,
	fps -> fpsDef,
	fov -> fovDef,
	mode->modeDef,
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
    exposure->exposureDef,
    bootMode->bootModeDef
    },
    "HERO3",{
	videoResolution -> videoResolutionDef,
	fps -> fpsDef,
	fov -> fovDef,
	mode->modeDef,
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
    bootMode->bootModeDef
    },
    "HERO2",{
    videoResolution -> videoResolutionDef,
	fov -> fovDef,
	fps -> fpsDef,
	mode->modeDef,
	videoMode->videoModeDef,
    photoResolution->photoResolutionDef,
    timeLapse->timeLapseDef,
    volume->volumeDef,
    led->ledDef,
    spotMeter->spotMeterDef,
    autoPowerOff->autoPowerOffDef,
    protune->protuneDef,
    bootMode->bootModeDef
    }
    
]


(*function goProSet[] enables camera setting through rules. The first thing to happen after usage of this function will be
	execution function downloadSettings[] to update variables. After updating variables function goProSet will test new values
	against values downloaed from camera and set only those parameters which are not the same. This precaution will prevent
	us from overwhelming camera with useless http requests.
 *)

goProSet[OptionsPattern[]] :=(If[goProPassword=="",Message[goProSetPassword::goProPassword, goProPassword],
	downloadSettings[];
    (*goProSetVideo[Flatten[{videoResolution->OptionValue[videoResolution],fps-> OptionValue[fps], fov->OptionValue[fov],videoMode->OptionValue[videoMode]}]];*)

	 (*I am using the test on SameQ so i won't do request which will set the same setting as which is already set*)
	 
	Switch[camera,"HERO3+",(
	goProSetVideoRes[OptionValue[videoResolution]];
	goProSetFPS[OptionValue[fps]];

    (*If[ MemberQ[goProGetPossibleFPS[OptionValue[videoResolution]], OptionValue[fps] ],

    ];*)
    If[!SameQ[OptionValue[fov],fovDef],goProSetFOV[OptionValue[fov]]];
    If[!SameQ[OptionValue[videoMode],videoModeDef],goProSetVideoMode[OptionValue[videoMode]]];
	 If[!SameQ[OptionValue[mode],modeDef],goProMode[OptionValue[mode]]];
	  If[!SameQ[OptionValue[bootMode],bootModeDef],goProSetBootMode[OptionValue[bootMode]]];

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
     ),
     "HERO3",(
	goProSetVideoRes[OptionValue[videoResolution]];
	goProSetFPS[OptionValue[fps]];

    (*If[ MemberQ[goProGetPossibleFPS[OptionValue[videoResolution]], OptionValue[fps] ],

    ];*)
    If[!SameQ[OptionValue[fov],fovDef],goProSetFOV[OptionValue[fov]]];
    If[!SameQ[OptionValue[videoMode],videoModeDef],goProSetVideoMode[OptionValue[videoMode]]];
	 If[!SameQ[OptionValue[mode],modeDef],goProMode[OptionValue[mode]]];
	  If[!SameQ[OptionValue[bootMode],bootModeDef],goProSetBootMode[OptionValue[bootMode]]];
	
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
     ),
     "HERO2",(
     goProSetVideoRes[OptionValue[videoResolution]];
     
     goProSetFPS[OptionValue[fps]];
    If[!SameQ[OptionValue[fov],fovDef],goProSetFOV[OptionValue[fov]]];
    If[!SameQ[OptionValue[videoMode],videoModeDef],goProSetVideoMode[OptionValue[videoMode]]];
	 If[!SameQ[OptionValue[mode],modeDef],goProMode[OptionValue[mode]]];
	  If[!SameQ[OptionValue[bootMode],bootModeDef],goProSetBootMode[OptionValue[bootMode]]];
     If[!SameQ[OptionValue[photoResolution],photoResolutionDef],goProSetPhotoRes[OptionValue[photoResolution]]];
     If[!SameQ[OptionValue[timeLapse],timeLapseDef],goProSetTimeLapse[OptionValue[timeLapse]]];
     If[!SameQ[OptionValue[volume],volumeDef],goProSetVolume[OptionValue[volume]]];
     If[!SameQ[OptionValue[led],ledDef],goProSetLed[OptionValue[led]]];
     If[!SameQ[OptionValue[spotMeter],spotMeterDef],goProSetSpotMeter[OptionValue[spotMeter]]];
     If[!SameQ[OptionValue[autoPowerOff],autoPowerOffDef],goProSetAutoPowerOff[OptionValue[autoPowerOff]]];
     If[!SameQ[OptionValue[protune],protuneDef],goProSetProtune[OptionValue[protune]]];
     )
	]
	]
    )

goProGetSettingReportAssociation[]:=(downloadSettings[];<|
	"videoResolution" -> videoResolutionDef,
	"mode"->modeDef,
	"fps" -> fpsDef,
	"fov" -> fovDef,
	"videoMode"->videoModeDef,
    "photoResolution"->photoResolutionDef,
    "loopVideo"->loopVideoDef,
    "burstRate"->burstRateDef,
    "timeLapse"->timeLapseDef,
    "continuousShot"->continuousShotDef,
    "photoInVideo"->photoInVideoDef,
    "volume"->volumeDef,
    "led"->ledDef,
    "lowLight"->lowLightDef,
    "spotMeter"->spotMeterDef,
    "autoPowerOff"->autoPowerOffDef,
    "protune"->protuneDef,
    "whiteBalance"->whiteBalanceDef,
    "colorProfile"->colorProfileDef,
    "iso"->isoDef,
    "sharpness"->sharpnessDef,
    "exposure"->exposureDef,
    "bootMode"->bootModeDef

|>)


vars={
	"videoResolution",
	"fps",
	"fov",
	"videoMode",
    "photoResolution",
    "loopVideo",
    "burstRate",
    "timeLapse",
    "continuousShot",
    "photoInVideo",
    "volume",
    "led",
    "lowLight",
    "spotMeter",
    "autoPowerOff",
    "protune",
    "whiteBalance",
    "colorProfile",
    "iso",
    "sharpness",
    "exposure",
    "mode",
    "bootMode"
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



goProGetVariables[]:=Switch[camera,"HERO3+",
	ToExpression[#]&/@{
	"videoResolution",
	"fps",
	"fov",
	"videoMode",
    "photoResolution",
    "loopVideo",
    "burstRate",
    "timeLapse",
    "continuousShot",
    "photoInVideo",
    "volume",
    "led",
    "lowLight",
    "spotMeter",
    "autoPowerOff",
    "protune",
    "whiteBalance",
    "colorProfile",
    "iso",
    "sharpness",
    "exposure",
    "mode",
    "bootMode"
	},
	"HERO3",
	ToExpression[#]&/@{
	"videoResolution",
	"fps",
	"fov",
	"videoMode",
    "photoResolution",
    "loopVideo",
    "burstRate",
    "timeLapse",
    "continuousShot",
    "photoInVideo",
    "volume",
    "led",
    "lowLight",
    "spotMeter",
    "autoPowerOff",
    "protune",
    "whiteBalance",
    "mode",
    "bootMode"
	},
	"HERO2",
	ToExpression[#]&/@{
	"videoResolution",
	"fov",
	"fps",
	"videoMode",
    "photoResolution",
    "timeLapse",
    "volume",
    "led",
    "spotMeter",
    "autoPowerOff",
    "protune",
    "mode",
    "bootMode"
	}
]
	



(* ::Subsection:: *)
(* Download *)
urlBase="http://10.5.5.9:8080/DCIM/100GOPRO/";
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
   
 (*If[Import[urlBase] == $Failed,Message[goProGetFileList::empty, empty],*)     
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

goProDeleteAll[]:=execute[goProMakeCommand["camera","DA"]]
goProDeleteLastFile[]:=execute[goProMakeCommand["camera","DL"]]
(*goProDeleteFile[name_String]:="1"*)




End[]

EndPackage[]
