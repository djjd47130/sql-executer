program SQLScriptExec;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  uDataModule in 'uDataModule.pas' {dmDataModule: TDataModule},
  uMain2 in 'uMain2.pas' {frmSqlExec2},
  uConnection in 'uConnection.pas' {frmConnection},
  uDatabases in 'uDatabases.pas' {frmDatabases},
  uOutputWindow in 'uOutputWindow.pas' {frmOutputWindow},
  uDatasetView in 'uDatasetView.pas' {frmDatasetView},
  uContentBase in 'uContentBase.pas' {frmContentBase},
  uContentScriptExec in 'uContentScriptExec.pas' {frmContentScriptExec},
  uContentHome in 'uContentHome.pas' {frmContentHome},
  uContentObject in 'uContentObject.pas' {frmContentObject},
  AppInit in 'AppInit.pas',
  SQLExec in 'SQLExec.pas',
  SQLExecThread in 'SQLExecThread.pas',
  SQLObjects in 'SQLObjects.pas',
  SQLConnections in 'SQLConnections.pas',
  SQLExecCommon in 'SQLExecCommon.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Light');
  Application.Title := 'SQL Script Executer';
  Application.CreateForm(TdmDataModule, dmDataModule);
  Application.CreateForm(TfrmSqlExec2, frmSqlExec2);
  Application.Run;
end.
