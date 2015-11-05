program SQLScriptExec;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {frmSqlExec},
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
  uContentHome in 'uContentHome.pas' {frmContentBase2},
  SqlExecThread in 'SqlExecThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Iceberg Classico');
  Application.Title := 'SQL Script Executer';
  RunApp;
  Application.CreateForm(TfrmSqlExec, frmSqlExec);
  Application.CreateForm(TfrmDatabases, frmDatabases);
  Application.Run;
end.
