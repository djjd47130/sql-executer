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
  uSplash in 'uSplash.pas' {frmSplash},
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
  frmSplash:= TfrmSplash.Create(nil);
  Application.CreateForm(TfrmSqlExec, frmSqlExec);
  Application.CreateForm(TfrmDatabases, frmDatabases);
  Application.CreateForm(TfrmDatasetView, frmDatasetView);
  Application.CreateForm(TfrmContentBase, frmContentBase);
  Application.CreateForm(TfrmContentScriptExec, frmContentScriptExec);
  Application.CreateForm(TfrmContentBase2, frmContentBase2);
  Application.Run;
end.
