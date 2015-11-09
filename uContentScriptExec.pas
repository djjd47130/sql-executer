unit uContentScriptExec;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uContentBase, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Buttons, SynEdit, Vcl.ExtCtrls,
  uOutputWindow, SynEditHighlighter, SynHighlighterSQL, System.Actions,
  Vcl.ActnList,
  SQLExecCommon,
  SQLConnections,
  uDatabases;

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
    procedure FormShow(Sender: TObject);
    procedure cboCurConnClick(Sender: TObject);
    procedure cboCurDatabaseClick(Sender: TObject);
  private
    FOutput: TfrmOutputWindow;
    FDatabases: TfrmDatabases;
    procedure AddConnection(AConn: TServerConnection);
    procedure DeleteConnection(AConn: TServerConnection);
    procedure AddDatabase(ADatabase: String);
    procedure DeleteDatabase(ADatabase: String);
  public
    procedure WndMethod(var Msg: TMessage); override;
    function CurConnection: TServerConnection;
  end;

var
  frmContentScriptExec: TfrmContentScriptExec;

implementation

uses
  uDataModule
  , uMain2
  ;

{$R *.dfm}

procedure TfrmContentScriptExec.FormCreate(Sender: TObject);
begin
  inherited;
  ED.Align:= alClient;
  FDatabases:= TfrmDatabases.Create(nil);
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
  FDatabases.Free;
end;

procedure TfrmContentScriptExec.FormShow(Sender: TObject);
begin
  inherited;
  actRefreshConnections.Execute;
end;

procedure TfrmContentScriptExec.actRefreshConnectionsExecute(Sender: TObject);
var
  X: Integer;
  C: TServerConnection;
begin
  inherited;
  cboCurConn.Items.Clear;
  cboCurConn.Items.Add('[Select Connection]');
  for X := 0 to frmSqlExec2.Connections.Count-1 do begin
    C:= frmSqlExec2.Connections[X];
    AddConnection(C);
  end;
  if cboCurConn.Items.Count > 1 then
    cboCurConn.ItemIndex:= 1
  else
    cboCurConn.ItemIndex:= 0;
  cboCurConnClick(nil);
end;

procedure TfrmContentScriptExec.actRefreshDatabasesExecute(Sender: TObject);
var
  C: TServerConnection;
  D: TServerDatabase;
  X: Integer;
  S: String;
begin
  inherited;
  cboCurDatabase.Items.Clear;
  cboCurDatabase.Items.Add('[Select Database]');
  C:= Self.CurConnection;
  if Assigned(C) then begin
    for X := 0 to C.DatabaseCount-1 do begin
      D:= C.Databases[X];
      cboCurDatabase.Items.AddObject(D.Name, D);
    end;
    cboCurDatabase.Items.Add('[Multiple Selected]');
    if C.SelDatabases.Count > 0 then begin
      if C.SelDatabases.Count = 1 then begin
        S:= C.SelDatabases[0];
        cboCurDatabase.ItemIndex:= cboCurDatabase.Items.IndexOf(S);
      end else begin
        //TODO: Multiple selected
        cboCurDatabase.ItemIndex:= cboCurDatabase.Items.Count-1;
      end;
    end else begin
      if cboCurDatabase.Items.Count > 2 then
        cboCurDatabase.ItemIndex:= 1
      else
        cboCurDatabase.ItemIndex:= 0;
    end;
  end else begin
    cboCurDatabase.ItemIndex:= 0;
  end;
end;

procedure TfrmContentScriptExec.actBatchExecute(Sender: TObject);
var
  C: TServerConnection;
  X: Integer;
  S: String;
begin
  inherited;
  //Pick batch databases
  C:= CurConnection;
  if Assigned(C) then begin
    FDatabases.LoadDatabases(C);
    if FDatabases.ShowModal = mrOk then begin
      C.SelDatabases.Clear;
      if FDatabases.CheckedCount > 0 then begin
        for X := 0 to FDatabases.Lst.Count-1 do begin
          if FDatabases.Lst.Checked[X] then begin
            C.SelDatabases.Add(FDatabases.Lst.Items[X]);
          end;
        end;
        if C.SelDatabases.Count = 1 then begin
          cboCurDatabase.ItemIndex:= cboCurDatabase.Items.IndexOf(C.SelDatabases[0]);
        end else begin
          cboCurDatabase.ItemIndex:= cboCurDatabase.Items.Count-1;
        end;
      end else begin
        if cboCurDatabase.Items.Count > 2 then
          cboCurDatabase.ItemIndex:= 1
        else
          cboCurDatabase.ItemIndex:= 0;
      end;
    end;
  end;
end;

procedure TfrmContentScriptExec.actExecSqlExecute(Sender: TObject);
begin
  inherited;
  //Execute SQL
  //TODO: Add jobs to thread queue


  //TODO: Execute thread queue



end;

procedure TfrmContentScriptExec.AddConnection(AConn: TServerConnection);
begin
  if Assigned(AConn) then begin
    cboCurConn.Items.AddObject(AConn.ConnectionString['Data Source'], AConn);
    if cboCurConn.ItemIndex <= 0 then begin
      cboCurConn.ItemIndex:= 1;
    end;
  end;
  cboCurConnClick(nil);
end;

procedure TfrmContentScriptExec.DeleteConnection(AConn: TServerConnection);
var
  TC: TServerConnection;
  X: Integer;
begin
  for X := 0 to cboCurConn.Items.Count-1 do begin
    TC:= TServerConnection(cboCurConn.Items.Objects[X]);
    if Assigned(TC) then begin
      if TC = AConn then begin
        cboCurConn.Items.Delete(X);
        Break;
      end;
    end;
  end;
  if cboCurConn.Items.Count = 1 then
    cboCurConn.ItemIndex:= 0;
  cboCurConnClick(nil);
end;

procedure TfrmContentScriptExec.AddDatabase(ADatabase: String);
begin
  if cboCurDatabase.Items.IndexOf(ADatabase) < 0 then begin
    cboCurDatabase.Items.Add(ADatabase);
  end;
end;

procedure TfrmContentScriptExec.cboCurConnClick(Sender: TObject);
begin
  inherited;
  actRefreshDatabases.Execute;
end;

procedure TfrmContentScriptExec.cboCurDatabaseClick(Sender: TObject);
var
  C: TServerConnection;
begin
  inherited;
  //Selected Item in Database Dropdown
  C:= Self.CurConnection;
  if Assigned(C) then begin
    if cboCurDatabase.ItemIndex > 0 then begin
      if cboCurDatabase.ItemIndex = cboCurDatabase.Items.Count-1 then begin
        actBatch.Execute;
      end else begin
        C.SelDatabases.Text:= cboCurDatabase.Text;
      end;
    end else begin
      C.SelDatabases.Clear;
    end;
  end;
end;

function TfrmContentScriptExec.CurConnection: TServerConnection;
begin
  Result:= nil;
  if cboCurConn.ItemIndex > 0 then begin
    Result:= TServerConnection(cboCurConn.Items.Objects[cboCurConn.ItemIndex]);
  end;
end;

procedure TfrmContentScriptExec.DeleteDatabase(ADatabase: String);
begin
  if cboCurDatabase.Items.IndexOf(ADatabase) >= 0 then begin
    cboCurDatabase.Items.Delete(cboCurDatabase.Items.IndexOf(ADatabase));
  end;
end;

procedure TfrmContentScriptExec.WndMethod(var Msg: TMessage);
var
  C: TServerConnection;
  D: PChar;
begin
  if Msg.Msg = MSG_CONNECTION_ADD then begin
    C:= TServerConnection(Msg.WParam);
    AddConnection(C);
  end else
  if Msg.Msg = MSG_CONNECTION_DEL then begin
    C:= TServerConnection(Msg.WParam);
    DeleteConnection(C);
  end else
  if Msg.Msg = MSG_DATABASE_ADD then begin
    //Database Added
    D:= PChar(Msg.WParam);
    AddDatabase(D);
  end else
  if Msg.Msg = MSG_DATABASE_DEL then begin
    //Database Deleted
    D:= PChar(Msg.WParam);
    DeleteDatabase(D);
  end else begin
    Msg.Result := DefWindowProc(frmSqlExec2.Wnd, Msg.Msg, Msg.wParam, Msg.lParam);
  end;
end;

end.
