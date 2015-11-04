unit AppInit;

(*
  AppInit.pas - Controls application startup
  (NOT YET IMPLEMENTED)

  "RunApp" is the core for running the application. Certain tasks
    which have to be performed cannot be done within the project
    main file - therefore it has been migrated to here. However, once
    migrated here, certain Delphi IDE features become disabled (such
    as selecting styles, version info, etc.). So this will be one of
    the last things implemented.
*)

interface

procedure RunApp;

implementation

procedure RunApp;
begin
  //TODO: Implement "OPEN" verb when opening text file from another application

  //TODO: Implement params to detect silent execution, etc.
    //Use param names similar to those used in OSQL

  //TODO: Implement running in console mode

end;

end.
