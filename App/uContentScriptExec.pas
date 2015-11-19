unit uContentScriptExec;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  System.UITypes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uContentBase, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Buttons, SynEdit, Vcl.ExtCtrls,
  uOutputWindow, SynEditHighlighter, SynHighlighterSQL, System.Actions,
  Vcl.ActnList,
  Data.DB,
  SQLExec,
  SQLExecThread,
  SQLExecCommon,
  SQLConnections,
  uDatabases, Vcl.ExtDlgs,
  ChromeTabs,
  ChromeTabsTypes,
  ChromeTabsUtils,
  ChromeTabsControls,
  ChromeTabsThreadTimer,
  ChromeTabsClasses;

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
    actSave: TAction;
    actSaveAs: TAction;
    dlgSave: TSaveTextFileDialog;
    actUndo: TAction;
    actFont: TAction;
    dlgFont: TFontDialog;
    Prog: TProgressBar;
    tmrStatus: TTimer;
    txtSplitWord: TEdit;
    Label3: TLabel;
    tmrChange: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actExecSqlExecute(Sender: TObject);
    procedure actBatchExecute(Sender: TObject);
    procedure actRefreshDatabasesExecute(Sender: TObject);
    procedure actRefreshConnectionsExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cboCurConnClick(Sender: TObject);
    procedure cboCurDatabaseClick(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure EDChange(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actFontExecute(Sender: TObject);
    procedure StatDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure tmrStatusTimer(Sender: TObject);
    procedure tmrChangeTimer(Sender: TObject);
    procedure txtSplitWordChange(Sender: TObject);
  private
    FOutput: TfrmOutputWindow;
    FDatabases: TfrmDatabases;
    FExec: TSqlExec;
    FStatus: TSQLThreadStatus;
    FFilename: String;
    FIsNew: Boolean;
    FIsChanged: Boolean;
    FCurrentBlock: Integer;
    FTotalBlocks: Integer;
    FCurrentJob: Integer;
    FTotalJobs: Integer;
    procedure AddConnection(AConn: TServerConnection);
    procedure DeleteConnection(AConn: TServerConnection);
    procedure AddDatabase(ADatabase: String);
    procedure DeleteDatabase(ADatabase: String);
    procedure RefreshActions;
    function StartNewThread: Boolean;
    procedure ThreadStatus(Sender: TSQLExecThread; const Status: TSQLThreadStatus);
    procedure ThreadJobEnded(Sender: TSQLExecThread; const Job: TSQLThreadJob);
    procedure ThreadJobStarted(Sender: TSQLExecThread; const Job: TSQLThreadJob);
    procedure ThreadWork(Sender: TSQLExecThread; const CurrentBlock,
      TotalBlocks, CurrentJob, TotalJobs: Integer);
    procedure ThreadBlockMsg(Sender: TSQLExecThread; const Job: TSQLThreadJob;
      const Block: TSqlExecBlock; const Msg: String);
    procedure ThreadDataset(Sender: TSQLExecThread; const Job: TSQLThreadJob;
      const Dataset: TDataset);
    procedure RefreshCaption;
    procedure UpdateProgressBar;
  public
    procedure WndMethod(var Msg: TMessage); override;
    function CurConnection: TServerConnection;
    procedure LoadFromFile(const AFilename: String);
    function PromptClose: Boolean;
    procedure PostLog(const S: String; const Style: TFontStyles = [];
      const Color: TColor = clDefault; const Detail: String = '');
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
var
  ProgressBarStyle: integer;
begin
  inherited;
  ED.Align:= alClient;

  //Set progress bar in status bar
  Stat.Panels[4].Style := psOwnerDraw;
  Prog.Parent := Stat;
  ProgressBarStyle := GetWindowLong(Prog.Handle, GWL_EXSTYLE);
  ProgressBarStyle := ProgressBarStyle - WS_EX_STATICEDGE;
  SetWindowLong(Prog.Handle, GWL_EXSTYLE, ProgressBarStyle);
  Prog.Visible:= False;

  FExec:= TSqlExec.Create(nil);
  FDatabases:= TfrmDatabases.Create(nil);
  FOutput:= TfrmOutputWindow.Create(nil);
  FOutput.BorderStyle:= TFormBorderStyle.bsNone;
  FOutput.Parent:= pOutput;
  FOutput.Align:= alClient;
  FOutput.Show;
  pOutput.Height:= 300;
  FFilename:= '';
  FIsNew:= True;
  FIsChanged:= False;
  Stat.Panels[2].Text:= '';
  tmrChange.Enabled:= False;
  tmrChange.Enabled:= True;
end;

procedure TfrmContentScriptExec.FormDestroy(Sender: TObject);
begin
  inherited;
  FOutput.Free;
  FDatabases.Free;
  FExec.Free;
end;

procedure TfrmContentScriptExec.FormShow(Sender: TObject);
begin
  inherited;
  actRefreshConnections.Execute;
  RefreshActions;
end;

procedure TfrmContentScriptExec.RefreshCaption;
begin
  if FIsNew then begin
    Caption:= 'New Script';
  end else begin
    Caption:= ExtractFileName(FFilename)+' ('+ExtractFilePath(FFilename)+')';
  end;
end;

procedure TfrmContentScriptExec.LoadFromFile(const AFilename: String);
begin
  ED.Lines.LoadFromFile(AFilename);
  FFilename:= AFilename;
  FIsNew:= False;
  FIsChanged:= False;
  RefreshCaption;
  tmrChange.Enabled:= False;
  tmrChange.Enabled:= True;
end;

procedure TfrmContentScriptExec.actFontExecute(Sender: TObject);
begin
  inherited;
  dlgFont.Font.Assign(ED.Font);
  if dlgFont.Execute then begin
    ED.Font.Assign(dlgFont.Font);
  end;
end;

procedure TfrmContentScriptExec.actExecSqlExecute(Sender: TObject);
begin
  inherited;
  Screen.Cursor:= crHourglass;
  try
    FOutput.ClearAll;
    FOutput.SetFocus;
    FOutput.Tabs.ActiveTabIndex:= 0;
    FOutput.OutputBox.SetFocus;
    StartNewThread;
  finally
    Screen.Cursor:= crDefault;
  end;
  RefreshActions;
end;

function TfrmContentScriptExec.StartNewThread: Boolean;
var
  T: TSQLExecThread;
  J: TSQLThreadJob;
  C: TServerConnection;
  X: Integer;
  CS: TConnectionString;
begin
  //Execute SQL
  Result:= False;
  C:= CurConnection;
  if Assigned(C) then begin
    T:= TSQLExecThread.Create;
    try
      //Set event handlers
      T.OnStatus:= ThreadStatus;
      T.OnJobStart:= ThreadJobStarted;
      T.OnJobEnd:= ThreadJobEnded;
      T.OnBlockMsg:= ThreadBlockMsg;
      T.OnWork:= ThreadWork;
      T.OnDataset:= ThreadDataset;
      //Populate jobs
      for X := 0 to Self.FDatabases.TotalCount-1 do begin
        if FDatabases.Selected[X] then begin
          CS:= String(C.ConnectionString);
          CS['Initial Catalog']:= FDatabases[X];
          J:= T.AddToQueue;
          J.ConnStr:= CS;
          J.SplitWord:= txtSplitWord.Text;
          if ED.SelLength > 0 then
            J.SQL.Text:= ED.SelText
          else
            J.SQL.Assign(ED.Lines);
          J.ExecMode:= TSQLExecMode(cboCurExecMethod.ItemIndex);
        end;
      end;
    finally
      T.Start;
      Result:= True;
    end;
  end else begin
    //No server selected

  end;
end;

procedure TfrmContentScriptExec.StatDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  inherited;
  if Panel = StatusBar.Panels[4] then
  with Prog do begin
    Top := Rect.Top;
    Left := Rect.Left;
    Width := Rect.Right - Rect.Left - 15;
    Height := Rect.Bottom - Rect.Top;
  end;
end;

procedure TfrmContentScriptExec.tmrChangeTimer(Sender: TObject);
begin
  inherited;
  //Something changed in the edit control
  tmrChange.Enabled:= False;
  FExec.SQL.Assign(ED.Lines);
  FExec.SplitWord:= Self.txtSplitWord.Text;
  FExec.ParseSQL;
  Stat.Panels[3].Text:= IntToStr(FExec.BlockCount)+' Block(s)';
  RefreshActions;
end;

procedure TfrmContentScriptExec.tmrStatusTimer(Sender: TObject);
begin
  inherited;

  Stat.Panels[1].Text:= 'Ln '+IntToStr(ED.CaretY)+' Col '+IntToStr(ED.CaretX);
  UpdateProgressBar;
end;

procedure TfrmContentScriptExec.txtSplitWordChange(Sender: TObject);
begin
  inherited;
  FExec.SplitWord:= Self.txtSplitWord.Text;
  FExec.SQL.Assign(ED.Lines);
  Stat.Panels[3].Text:= IntToStr(FExec.BlockCount)+' Blocks';
end;

procedure TfrmContentScriptExec.UpdateProgressBar;
begin
  case FStatus of
    esReady: begin
      //Thread is ready for new jobs
      Prog.Visible:= False;
    end;
    esBusy: begin
      //Thread is currently busy processing jobs
      Prog.Visible:= True;
      Prog.State:= TProgressBarState.pbsNormal;
    end;
    esError: begin
      //Thread is in an unrecoverable error state
      Prog.Visible:= True;
      Prog.State:= TProgressBarState.pbsError;
    end;
  end;
  if Prog.Visible then begin
    if Prog.Max <> FTotalBlocks then
      Prog.Max:= FTotalBlocks;
    if Prog.Position <> FCurrentBlock then
      Prog.Position:= FCurrentBlock;
  end;
end;

procedure TfrmContentScriptExec.ThreadStatus(Sender: TSQLExecThread;
  const Status: TSQLThreadStatus);
begin
  FStatus:= Status;
  case FStatus of
    esBusy: begin
      PostLog('');
      PostLog('Started execution...', [fsBold], clGreen);
      Tab.SpinnerState:= TChromeTabSpinnerState.tssRenderedDownload;
    end;
    esReady: begin
      PostLog('');
      PostLog('Finished execution of '+IntToStr(FTotalJobs)+' Jobs', [fsBold], clGreen);
      Tab.SpinnerState:= TChromeTabSpinnerState.tssNone;
    end;
    esError: begin
      PostLog('');
      PostLog('SQL EXEC ERROR', [fsBold], clRed);
      Tab.SpinnerState:= TChromeTabSpinnerState.tssNone;
    end;
  end;
end;

procedure TfrmContentScriptExec.ThreadWork(Sender: TSQLExecThread;
  const CurrentBlock, TotalBlocks, CurrentJob, TotalJobs: Integer);
begin
  FCurrentBlock:= CurrentBlock;
  FTotalBlocks:= TotalBlocks;
  FCurrentJob:= CurrentJob;
  FTotalJobs:= TotalJobs;
end;

procedure TfrmContentScriptExec.ThreadJobStarted(Sender: TSQLExecThread; const Job: TSQLThreadJob);
begin
  PostLog('');
  PostLog('Job Starting on '+Job.ConnStr['Data Source']+'\'+Job.ConnStr['Initial Catalog'], [fsBold], clNavy);
end;

procedure TfrmContentScriptExec.ThreadBlockMsg(Sender: TSQLExecThread;
  const Job: TSQLThreadJob; const Block: TSqlExecBlock; const Msg: String);
begin
  PostLog(' > Message: '+Msg, [fsItalic], clBlue);
end;

procedure TfrmContentScriptExec.ThreadDataset(Sender: TSQLExecThread;
  const Job: TSQLThreadJob; const Dataset: TDataset);
begin
  FOutput.AddDataset(Job, Dataset);
end;

procedure TfrmContentScriptExec.ThreadJobEnded(Sender: TSQLExecThread; const Job: TSQLThreadJob);
begin
  PostLog('Job Complete on '+Job.ConnStr['Data Source']+'\'+Job.ConnStr['Initial Catalog'], [fsBold], clNavy);
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
  RefreshActions;
end;

procedure TfrmContentScriptExec.actRefreshDatabasesExecute(Sender: TObject);
var
  C: TServerConnection;
  D: TServerDatabase;
  X: Integer;
  S: String;
begin
  inherited;
  C:= Self.CurConnection;
  cboCurDatabase.Items.Clear;
  cboCurDatabase.Items.Add('[Select Database]');
  if Assigned(C) then begin
    Stat.Panels[0].Text:= 'Connected';
    FDatabases.LoadDatabases(C);
    for X := 0 to C.DatabaseCount-1 do begin
      D:= C.Databases[X];
      cboCurDatabase.Items.AddObject(D.Name, D);
    end;
    cboCurDatabase.Items.Add('[Multiple Selected]');
    if FDatabases.TotalCount = 0 then Exit;    
    if FDatabases.CheckedCount > 0 then begin
      if FDatabases.TotalCount = 1 then begin
        S:= FDatabases.Items[0];
        cboCurDatabase.ItemIndex:= cboCurDatabase.Items.IndexOf(S);
      end else begin
        cboCurDatabase.ItemIndex:= cboCurDatabase.Items.Count-1;
      end;
    end else begin
      if cboCurDatabase.Items.Count > 2 then
        cboCurDatabase.ItemIndex:= 1
      else
        cboCurDatabase.ItemIndex:= 0;
    end;
  end else begin
    Stat.Panels[0].Text:= 'Disconnected';
    cboCurDatabase.ItemIndex:= 0;
  end;
  RefreshActions;
  cboCurDatabaseClick(nil);
end;

procedure TfrmContentScriptExec.actSaveAsExecute(Sender: TObject);
var
  FN: String;
  T: String;
begin
  inherited;
  dlgSave.FileName:= FFilename;
  if dlgSave.Execute then begin
    FN:= dlgSave.FileName;
    case dlgSave.FilterIndex of
      1: begin
        //SQL Script File
        T:= Copy(FN, Length(FN)-3, 4);
        if not SameText(T, '.sql') then
          FN:= FN + '.sql';
      end;
      2: begin
        //TXT Plain Text File
        T:= Copy(FN, Length(FN)-3, 4);
        if not SameText(T, '.txt') then
          FN:= FN + '.txt';
      end;
      else begin
        //All Files (Any Extension)

      end;
    end;
    ED.Lines.SaveToFile(FN);
    FIsNew:= False;
    FIsChanged:= False;
    FFilename:= FN;
    frmSqlExec2.AddFileToRecents(FFilename);
    Stat.Panels[2].Text:= '';
  end;
  RefreshActions;
end;

procedure TfrmContentScriptExec.actSaveExecute(Sender: TObject);
begin
  inherited;
  if FIsNew then begin
    actSaveAs.Execute;
  end else begin
    if DirectoryExists(ExtractFilePath(FFilename)) then begin
      ED.Lines.SaveToFile(FFilename);
      FIsChanged:= False;
      Stat.Panels[2].Text:= '';
    end else begin
      raise Exception.Create('Directory does not exist');
    end;
  end;
  RefreshActions;
end;

procedure TfrmContentScriptExec.actUndoExecute(Sender: TObject);
begin
  inherited;
  ED.Undo;
  actUndo.Enabled:= ED.CanUndo;
  RefreshActions;
end;

function TfrmContentScriptExec.PromptClose: Boolean;
var
  F: String;
begin
  Result:= False;
  if FStatus = esBusy then Exit;
  if FIsChanged then begin
    if FIsNew then F:= 'New File' else F:= '"'+ExtractFileName(FFilename)+'"';
    case MessageDlg('Would you like to save changes to '+F+'?', mtConfirmation, [mbYes,mbNo,mbCancel], 0) of
      mrYes: begin
        actSave.Execute;
        Result:= not FIsChanged;
      end;
      mrNo: begin
        Result:= True;
      end;
    end;
  end else begin
    Result:= True;
  end;
end;

procedure TfrmContentScriptExec.actBatchExecute(Sender: TObject);
var
  C: TServerConnection;
  X: Integer;
begin
  inherited;
  //Pick batch databases
  C:= CurConnection;
  if Assigned(C) then begin
    if FDatabases.ShowModal = mrOk then begin
      case FDatabases.CheckedCount of
        0: begin
          cboCurDatabase.ItemIndex:= 0;
        end;
        1: begin
          for X := 0 to FDatabases.TotalCount-1 do begin
            if FDatabases.Selected[X] then begin
              cboCurDatabase.ItemIndex:= cboCurDatabase.Items.IndexOf(FDatabases.Items[X]);
              Break;
            end;
          end;
        end;
        else begin
          cboCurDatabase.ItemIndex:= cboCurDatabase.Items.Count-1;
        end;
      end;
    end;
  end;
  RefreshActions;
end;

procedure TfrmContentScriptExec.RefreshActions;
var
  C: TServerConnection;
begin
  C:= CurConnection;
  actFont.Enabled:= (FStatus = esReady);
  actSave.Enabled:= (FStatus = esReady) and (FIsChanged);
  actUndo.Enabled:= (FStatus = esReady) and ED.CanUndo;
  actRefreshDatabases.Enabled:= Assigned(C) and (FStatus = esReady);
  actBatch.Enabled:= Assigned(C) and (FStatus = esReady);
  actExecSql.Enabled:= Assigned(C) and (FStatus = esReady);
  cboCurDatabase.Enabled:= Assigned(C) and (FStatus = esReady);
  cboCurExecMethod.Enabled:= Assigned(C) and (FStatus = esReady);
  cboCurConn.Enabled:= (cboCurConn.Items.Count > 1);
  if cboCurConn.Items.Count = 1 then
    cboCurConn.ItemIndex:= 0;
  if cboCurDatabase.Items.Count = 1 then
    cboCurDatabase.ItemIndex:= 0;

  frmSqlExec2.RefreshActions; //TODO: Decouple

  if cboCurConn.ItemIndex < 1 then
    Stat.Panels[0].Text:= 'Disconnected';

  RefreshCaption;
end;

procedure TfrmContentScriptExec.AddConnection(AConn: TServerConnection);
begin
  if Assigned(AConn) then begin
    cboCurConn.Items.AddObject(AConn.ConnectionString['Data Source'], AConn);
    if cboCurConn.ItemIndex < 1 then begin
      cboCurConn.ItemIndex:= 1;
    end;
  end;
  RefreshActions;
  actRefreshDatabases.Execute;
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
  if cboCurConn.Items.Count > 1 then
    cboCurConn.ItemIndex:= 1
  else
    cboCurConn.ItemIndex:= 0;
  cboCurConnClick(nil);
  RefreshActions;
end;

procedure TfrmContentScriptExec.AddDatabase(ADatabase: String);
begin
  if cboCurDatabase.Items.IndexOf(ADatabase) < 0 then begin
    cboCurDatabase.Items.Add(ADatabase);
  end;
  RefreshActions;
end;

procedure TfrmContentScriptExec.DeleteDatabase(ADatabase: String);
begin
  if cboCurDatabase.Items.IndexOf(ADatabase) >= 0 then begin
    cboCurDatabase.Items.Delete(cboCurDatabase.Items.IndexOf(ADatabase));
  end;
  RefreshActions;
end;

procedure TfrmContentScriptExec.EDChange(Sender: TObject);
begin
  inherited;
  FIsChanged:= True;
  Stat.Panels[2].Text:= 'Modified';
  tmrChange.Enabled:= False;
  tmrChange.Enabled:= True;
end;

procedure TfrmContentScriptExec.cboCurConnClick(Sender: TObject);
begin
  inherited;
  RefreshActions;
  actRefreshDatabases.Execute;
end;

procedure TfrmContentScriptExec.cboCurDatabaseClick(Sender: TObject);
var
  X: Integer;
begin
  inherited;
  if cboCurDatabase.ItemIndex > 0 then begin
    //At least one database is selected
    if cboCurDatabase.ItemIndex = cboCurDatabase.Items.Count-1 then begin
      //Show option to choose batch
      if cboCurDatabase.Focused then
        actBatch.Execute;
    end else begin
      //Single database is selected
      for X := 0 to FDatabases.TotalCount-1 do begin
        FDatabases.Selected[X]:= SameText(FDatabases.Items[X], cboCurDatabase.Text);
      end;
    end;
  end else begin
    //No databases are selected
    for X := 0 to FDatabases.TotalCount-1 do begin
      FDatabases.Selected[X]:= False;
    end;
  end;
  RefreshActions;
end;

function TfrmContentScriptExec.CurConnection: TServerConnection;
begin
  Result:= nil;
  if cboCurConn.ItemIndex > 0 then begin
    Result:= TServerConnection(cboCurConn.Items.Objects[cboCurConn.ItemIndex]);
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

procedure TfrmContentScriptExec.PostLog(const S: String; const Style: TFontStyles;
  const Color: TColor; const Detail: String);
begin
  FOutput.PostMsg(S, Style, Color, Detail);
end;

end.
