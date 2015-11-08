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

uses
  Vcl.Forms,
  System.SysUtils
  {$IFDEF USE_SPLASH}
  , uSplash
  {$ENDIF}
  {$IFDEF USE_V2}
  , uMain2
  {$ENDIF}
  ;

procedure RunApp;

implementation

procedure RunApp;
begin
  //TODO: Implement "OPEN" verb when opening text file from another application

  //TODO: Implement params to detect silent execution, etc.
    //Use param names similar to those used in OSQL

  //TODO: Implement running in console mode

  {$IFDEF USE_SPLASH}
  frmSplash:= TfrmSplash.Create(nil);
  {$ENDIF}

  {$IFDEF USE_V2}
  Application.CreateForm(TfrmSqlExec2, frmSqlExec2);
  {$ENDIF}

end;

end.
