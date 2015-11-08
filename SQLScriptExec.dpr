program SQLScriptExec;

uses
  Vcl.Forms,
  SQLExec in 'SQLExec.pas',
  uConnection in 'uConnection.pas' {frmConnection},
  AppInit in 'AppInit.pas',
  Vcl.Themes,
  Vcl.Styles,
  uDatabases in 'uDatabases.pas' {frmDatabases},
  uDatasetView in 'uDatasetView.pas' {frmDatasetView},
  SQLConnections in 'SQLConnections.pas',
  uMain in 'uMain.pas' {frmSqlExec};

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
