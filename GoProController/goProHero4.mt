(* Wolfram Language Test file *)


(* ::Subsection:: *)
(* modes *)

Get["GoProController`"]
goProSetCamera["Hero4"]
goProSetCameraModel["Silver"]

Test[
	goProSetBootMode[#]&/@goProGetPossibleBootMode[],
	{"http://10.5.5.9/gp/gpControl/setting/53/0",
	"http://10.5.5.9/gp/gpControl/setting/53/1",
	"http://10.5.5.9/gp/gpControl/setting/53/2"},
	TestID->"Test-goProSetBootMode"
]
Test[
	goProSwitchProtuneOn[],
	"http://10.5.5.9/gp/gpControl/setting/34/1",
	TestID->"Test-goProSwitchProtuneOn"
	
	
]


Test[
	goProSwitchProtuneOff[],

	"http://10.5.5.9/gp/gpControl/setting/34/0"
	,
	TestID->"Test-goProSwitchProtuneOff"
]


Test[
	goProSetVideoWhiteBalance[#]&/@goProGetPossibleWhiteBalance[],
	{
	"http://10.5.5.9/gp/gpControl/setting/11/0",
	"http://10.5.5.9/gp/gpControl/setting/11/1",
	"http://10.5.5.9/gp/gpControl/setting/11/5",
	"http://10.5.5.9/gp/gpControl/setting/11/6",
	"http://10.5.5.9/gp/gpControl/setting/11/2",
	"http://10.5.5.9/gp/gpControl/setting/11/7",
	"http://10.5.5.9/gp/gpControl/setting/11/3",
	"http://10.5.5.9/gp/gpControl/setting/11/4"
	
	}
	,
	TestID->"Test-goProSetVideoWhiteBalance"
]