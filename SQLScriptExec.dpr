program SQLScriptExec;

uses
  Vcl.Forms,
  SQLExec in 'SQLExec.pas',
  uConnection in 'uConnection.pas' {frmConnection},
  AppInit in 'AppInit.pas',
  Vcl.Themes,
  Vcl.Styles,
  uDatabases in 'uDatabases.pas' {frmDatabases},
  uOutputWindow in 'uOutputWindow.pas' {frmOutputWindow},
  uDatasetView in 'uDatasetView.pas' {frmDatasetView},
  uContentBase in 'uContentBase.pas' {frmContentBase},
  uContentScriptExec in 'uContentScriptExec.pas' {frmContentScriptExec},
  uContentHome in 'uContentHome.pas' {frmContentHome},
  SQLExecThread in 'SQLExecThread.pas',
  SQLObjects in 'SQLObjects.pas',
  SQLConnections in 'SQLConnections.pas',
  uContentObject in 'uContentObject.pas' {frmContentObject},
  uMain2 in 'uMain2.pas' {frmSqlExec2},
  CmdSwitches in 'CmdSwitches.pas',
  uDataModule in 'uDataModule.pas' {dmDataModule: TDataModule},
  SQLExecCommon in 'SQLExecCommon.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Light');
  Application.Title := 'SQL Script Executer';
  RunApp;
  Application.CreateForm(TdmDataModule, dmDataModule);
  Application.CreateForm(TfrmSqlExec2, frmSqlExec2);
  Application.CreateForm(TfrmDatabases, frmDatabases);
  Application.Run;
end.
