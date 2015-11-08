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
  uMain in 'uMain.pas' {frmSqlExec},
  uContentObject in 'uContentObject.pas' {frmContentObject};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Iceberg Classico');
  Application.Title := 'SQL Script Executer';
  RunApp;
  Application.CreateForm(TfrmSqlExec, frmSqlExec);
  Application.CreateForm(TfrmDatabases, frmDatabases);
  Application.CreateForm(TfrmContentObject, frmContentObject);
  Application.Run;
end.
