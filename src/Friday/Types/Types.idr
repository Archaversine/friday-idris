module Friday.Types.Types 

public export 
data FridayType 
    = FString 
    | FInt 
    | FDouble 
    | FBool
    | FVoid
    | FFunc FridayType (List (String, FridayType))
    | IOFnc FridayType (List FridayType)

export 
Eq FridayType where 
    FString    == FString      = True
    FInt       == FInt         = True
    FDouble    == FDouble      = True
    FBool      == FBool        = True
    FVoid      == FVoid        = True
    FFunc t ts == FFunc t' ts' = assert_total (t == t' && ts == ts')
    IOFnc t ts == IOFnc t' ts' = assert_total (t == t' && ts == ts')
    _          == _            = False

export 
Show FridayType where 
    show FString   = "FString"
    show FInt      = "FInt"
    show FDouble   = "FDouble"
    show FBool     = "FBool"
    show FVoid     = "FVoid"
    show (FFunc t ts) = "(FFunc " ++ show t ++ assert_total (show ts) ++ ")"
    show (IOFnc t ts) = "(IOFnc " ++ show t ++ assert_total (show ts) ++ ")"
