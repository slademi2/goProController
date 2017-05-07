BeginTestSection["HERO4_Test"]

VerificationTest[(* 1 *)
	Map[Function[goProSetBootMode[Slot[1]]], goProGetPossibleBootMode[]]
	,
	List["http://10.5.5.9/gp/gpControl/setting/53/0", "http://10.5.5.9/gp/gpControl/setting/53/1", "http://10.5.5.9/gp/gpControl/setting/53/2"]	
]

VerificationTest[(* 2 *)
	Map[Function[goProSetBootMode[Slot[1]]], goProGetPossibleBootMode[]]
	,
	List["http://10.5.5.9/gp/gpControl/setting/53/0", "http://10.5.5.9/gp/gpControl/setting/53/1", "http://10.5.5.9/gp/gpControl/setting/53/2"]	
]

VerificationTest[(* 3 *)
	Times[goProSwitchProtuneOn[], goProSwitchProtuneOff[]]
	,
	Times["http://10.5.5.9/gp/gpControl/setting/34/1", "http://10.5.5.9/gp/gpControl/setting/34/0"]	
]

VerificationTest[(* 4 *)
	Times[Map[Function[goProSetVideoWhiteBalance[Slot[1]]], goProGetPossibleWhiteBalance[]], Map[Function[goProSetPhotoWhiteBalance[Slot[1]]], goProGetPossibleWhiteBalance[]], Map[Function[goProSetMultiShotWhiteBalance[Slot[1]]], goProGetPossibleWhiteBalance[]]]
	,
	Times[List["http://10.5.5.9/gp/gpControl/setting/11/0", "http://10.5.5.9/gp/gpControl/setting/11/1", "http://10.5.5.9/gp/gpControl/setting/11/5", "http://10.5.5.9/gp/gpControl/setting/11/6", "http://10.5.5.9/gp/gpControl/setting/11/2", "http://10.5.5.9/gp/gpControl/setting/11/7", "http://10.5.5.9/gp/gpControl/setting/11/3", "http://10.5.5.9/gp/gpControl/setting/11/4"], List["http://10.5.5.9/gp/gpControl/setting/22/0", "http://10.5.5.9/gp/gpControl/setting/22/1", "http://10.5.5.9/gp/gpControl/setting/22/5", "http://10.5.5.9/gp/gpControl/setting/22/6", "http://10.5.5.9/gp/gpControl/setting/22/2", "http://10.5.5.9/gp/gpControl/setting/22/7", "http://10.5.5.9/gp/gpControl/setting/22/3", "http://10.5.5.9/gp/gpControl/setting/22/4"], List["http://10.5.5.9/gp/gpControl/setting/35/0", "http://10.5.5.9/gp/gpControl/setting/35/1", "http://10.5.5.9/gp/gpControl/setting/35/5", "http://10.5.5.9/gp/gpControl/setting/35/6", "http://10.5.5.9/gp/gpControl/setting/35/2", "http://10.5.5.9/gp/gpControl/setting/35/7", "http://10.5.5.9/gp/gpControl/setting/35/3", "http://10.5.5.9/gp/gpControl/setting/35/4"]]	
]

VerificationTest[(* 5 *)
	Times[Map[Function[goProSetVideoColorProfile[Slot[1]]], goProGetPossibleColorProfile[]], Map[Function[goProSetPhotoColorProfile[Slot[1]]], goProGetPossibleColorProfile[]], Map[Function[goProSetMultiShotColorProfile[Slot[1]]], goProGetPossibleColorProfile[]]]
	,
	Times[List["http://10.5.5.9/gp/gpControl/setting/12/0", "http://10.5.5.9/gp/gpControl/setting/12/1"], List["http://10.5.5.9/gp/gpControl/setting/23/0", "http://10.5.5.9/gp/gpControl/setting/23/1"], List["http://10.5.5.9/gp/gpControl/setting/36/0", "http://10.5.5.9/gp/gpControl/setting/36/1"]]	
]

EndTestSection[]
