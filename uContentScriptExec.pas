unit uContentScriptExec;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uContentBase, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Buttons, SynEdit, Vcl.ExtCtrls,
  uOutputWindow, SynEditHighlighter, SynHighlighterSQL, System.Actions,
  Vcl.ActnList;

type
  TfrmContentScriptExec = class(TfrmContentBase)
    pMain: TPanel;
    Splitter3: TSplitter;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    cboCurConn: TComboBox;
    cboCurDatabase: TComboBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cboCurExecMethod: TComboBox;
    ED: TSynEdit;
    pMessages: TPanel;
    Panel3: TPanel;
    Label3: TLabel;
    cmdCloseMessages: TSpeedButton;
    SynSQLSyn1: TSynSQLSyn;
    actRefreshConnections: TAction;
    actRefreshDatabases: TAction;
    actBatch: TAction;
    actExecSql: TAction;
    Stat: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actExecSqlExecute(Sender: TObject);
    procedure actBatchExecute(Sender: TObject);
    procedure actRefreshDatabasesExecute(Sender: TObject);
    procedure actRefreshConnectionsExecute(Sender: TObject);
  private
    FOutput: TfrmOutputWindow;
  public
    { Public declarations }
  end;

var
  frmContentScriptExec: TfrmContentScriptExec;

implementation

uses
  uMain;

{$R *.dfm}

procedure TfrmContentScriptExec.actRefreshConnectionsExecute(Sender: TObject);
begin
  inherited;
  //Refresh Connections

end;

procedure TfrmContentScriptExec.actRefreshDatabasesExecute(Sender: TObject);
begin
  inherited;
  //Refresh Databases

end;

procedure TfrmContentScriptExec.actBatchExecute(Sender: TObject);
begin
  inherited;
  //Pick batch databases

end;

procedure TfrmContentScriptExec.actExecSqlExecute(Sender: TObject);
begin
  inherited;
  //Execute SQL

end;

procedure TfrmContentScriptExec.FormCreate(Sender: TObject);
begin
  inherited;
  pMain.Align:= alClient;
  ED.Align:= alClient;
  FOutput:= TfrmOutputWindow.Create(nil);
  FOutput.BorderStyle:= TFormBorderStyle.bsNone;
  FOutput.Parent:= pMessages;
  FOutput.Align:= alClient;
  FOutput.Show;
end;

procedure TfrmContentScriptExec.FormDestroy(Sender: TObject);
begin
  inherited;
  FOutput.Free;
end;

end.
