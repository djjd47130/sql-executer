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
    SynSQL: TSynSQLSyn;
    actRefreshConnections: TAction;
    actRefreshDatabases: TAction;
    actBatch: TAction;
    actExecSql: TAction;
    pOutput: TPanel;
    pOutputTitle: TPanel;
    lblOutputTitle: TLabel;
    cmdOutputClose: TSpeedButton;
    Stat: TStatusBar;
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
    Action1: TAction;
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
  uDataModule
  , uMain2
  ;

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
  //TODO: Add jobs to thread queue


  //TODO: Execute thread queue



end;

procedure TfrmContentScriptExec.FormCreate(Sender: TObject);
begin
  inherited;
  ED.Align:= alClient;
  FOutput:= TfrmOutputWindow.Create(nil);
  FOutput.BorderStyle:= TFormBorderStyle.bsNone;
  FOutput.Parent:= pOutput;
  FOutput.Align:= alClient;
  FOutput.Show;
  pOutput.Height:= 170;
end;

procedure TfrmContentScriptExec.FormDestroy(Sender: TObject);
begin
  inherited;
  FOutput.Free;
end;

end.
