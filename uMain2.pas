unit uMain2;

(*
  JD SQL Script Executer
  by Jerry Dodge - started 10/29/2015

  Executes large SQL Script files on multiple databases at once

  Features:
  - Load multiple connections to different SQL Servers
  - Execute script file containing series of "GO" statements
    - Splits execution at "GO" statements in different "Blocks"
  - Error reporting (more accurate than MS Tools)
    - SQL Script included in errors
  - Much faster than Microsoft tools (because of little parsing)
  - Syntax highlighting using SynEdit control
  - Batch execution of script on multiple databases at once
  - Save/load recent server connections upon connecting
  - Browse databases, tables, stored procs, etc. (IN PROGRESS)

  TODO:
  - Implement tabular document interface (MAJOR)
    - Use ChromeTabs control
    - Currently in progress
      - uContentBase.pas has ancester form inherited by any content form
      - Chrome Tabs to control content form display
      - Chrome Tabs to also be used for any tabs
    - Different types of tab content
      - Home View (New File, Recent, etc.)
        - uContentHome.pas
      - Script File View (Plus result messages / data)
        - uContentScriptExec.pas
      - Server Detail View (Databases, etc.)
      - Database Detail View (Tables, etc.)
      - Table Detail View (Columns, etc.)
      - Stored Procedure View
      - Database Backup Scheduling
  - Implement tree view browsing databases, tables, stored procs, etc. (MAJOR)
    - Tree view along left side
    - Expand nodes to view more details of Tables, Stored Procs, etc.
    - Requires tabular document interface before opening detailed info
  - Implement selected tree view object details
    - Server Connection
    - Database
    - Table
    - Stored Procedure
    - Options
  - Implement tree view right-click menu
  - Implement find / replace functionality
    - Find First
    - Find Next
    - Find All
    - Replace
    - Replace All
  - Implement Edit menu - Cut / Copy / Paste / Delete
  - Implement Edit menu - GoTo
  - Implement Drag/Drop Open File(s)
  - Implement showing datasets
    - Partially implemented, needs revision
  - Implement recent documents selection (Menu)
    - Partially implemented, not yet working
  - Implmenet script block view
    - List individual blocks, the first line of script, errors, etc.
  - Implement help menu
  - Change progress bar to be for all databases, not just current
  - Change Save As to automatically include filename extension
  - Fix total lines affected count
  - Implement "USES" statement
  - Implement "PRINT" statement
  - Implement backup schedules
    - Requires service to be built first
  - Database server service application
    - Performs scheduled maintenance on databases
  - Implement Toolboxes for smaller portions of content
  - Monitor file date/time for changes

*)

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.ShellApi, Winapi.ShlObj,
  Winapi.ActiveX, Winapi.OleDB,

  System.SysUtils, System.Variants, System.Types, System.UITypes,
  System.Classes, System.Generics.Collections, System.Actions,
  System.Win.Registry, System.Win.ComObj,

  Data.DB, Data.Win.ADODB, Datasnap.DBClient, MidasLib,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ComCtrls,
  Vcl.ToolWin, Vcl.StdCtrls,  Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnMan, Vcl.ImgList, Vcl.ExtCtrls, Vcl.ExtDlgs, Vcl.Buttons, Vcl.Grids,
  Vcl.JumpList, Vcl.DBGrids,

  SQLExec, SQLExecThread,
  uDatasetView,
  SQLConnections,

  SynEdit, SynEditHighlighter, SynHighlighterSQL,
  SynMemo, SynHighlighterPas, SynEditMiscClasses, SynEditSearch,

  ChromeTabs,

  ChromeTabsTypes,
  ChromeTabsUtils,
  ChromeTabsControls,
  ChromeTabsThreadTimer,
  ChromeTabsClasses;

const
  REG_KEY = 'Software\JD Software\SqlScriptExec\';
  REG_KEY_RECENT_CONN = 'Software\JD Software\SqlScriptExec\RecentConn\';


type
  TfrmSqlExec2 = class(TForm)
    Stat: TStatusBar;
    MM: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    Server1: TMenuItem;
    Help1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    N1: TMenuItem;
    Connection1: TMenuItem;
    Acts: TActionManager;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actServerConnect: TAction;
    actFileExit: TAction;
    Exit1: TMenuItem;
    actEditUndo: TAction;
    actEditCut: TAction;
    actEditCopy: TAction;
    actEditPaste: TAction;
    actEditDelete: TAction;
    actEditFind: TAction;
    actEditFindNext: TAction;
    Undo1: TMenuItem;
    N3: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    N4: TMenuItem;
    Find1: TMenuItem;
    FindNext1: TMenuItem;
    actEditReplace: TAction;
    actEditGoTo: TAction;
    actEditSelectAll: TAction;
    Rreplace1: TMenuItem;
    GoTo1: TMenuItem;
    N5: TMenuItem;
    SelectAll1: TMenuItem;
    Font1: TMenuItem;
    actScriptFont: TAction;
    Imgs16: TImageList;
    Imgs24: TImageList;
    Imgs32: TImageList;
    Imgs48: TImageList;
    pMain: TPanel;
    dlgOpen: TOpenTextFileDialog;
    dlgSave: TSaveTextFileDialog;
    dlgFont: TFontDialog;
    pLeft: TPanel;
    Splitter1: TSplitter;
    TB: TToolBar;
    cmdNewFile: TToolButton;
    cmdOpenFile: TToolButton;
    cmdSaveFile: TToolButton;
    ToolButton4: TToolButton;
    cmdUndo: TToolButton;
    cmdFind: TToolButton;
    cmdFont: TToolButton;
    Script1: TMenuItem;
    ExecuteScript1: TMenuItem;
    N6: TMenuItem;
    Splitter2: TSplitter;
    Disconnect1: TMenuItem;
    actServerDisconnect: TAction;
    ToolButton7: TToolButton;
    actScriptExecute: TAction;
    pScript: TPanel;
    SynSQLSyn1: TSynSQLSyn;
    JumpList1: TJumpList;
    pSelected: TPanel;
    Panel4: TPanel;
    lblSelectedObject: TLabel;
    SpeedButton1: TSpeedButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    SelView: TStringGrid;
    pConnections: TPanel;
    Panel1: TPanel;
    Label4: TLabel;
    SpeedButton2: TSpeedButton;
    ToolBar1: TToolBar;
    ToolButton16: TToolButton;
    ToolButton5: TToolButton;
    TV: TTreeView;
    View1: TMenuItem;
    ShowConnections1: TMenuItem;
    ShowSelectedObject1: TMenuItem;
    ShowMessages1: TMenuItem;
    Prog: TProgressBar;
    actScriptBatch: TAction;
    Search: TSynEditSearch;
    actEditFindPrev: TAction;
    cmdFindPrev: TToolButton;
    cmdFindNext: TToolButton;
    cmdFindReplace: TToolButton;
    FindPrevious1: TMenuItem;
    ShowLinesAffected1: TMenuItem;
    Tabs: TChromeTabs;
    OutputWindow1: TMenuItem;
    ScriptWindow1: TMenuItem;
    mRecent: TMenuItem;
    N2: TMenuItem;
    tmrFileChange: TTimer;
    ToolButton12: TToolButton;
    ToolButton14: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actServerConnectExecute(Sender: TObject);
    procedure actServerDisconnectExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actScriptExecuteExecute(Sender: TObject);
    procedure TVClick(Sender: TObject);
    procedure TVExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure StatDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure cboCurConnClick(Sender: TObject);
    procedure cboCurDatabaseClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure View1Click(Sender: TObject);
    procedure ShowConnections1Click(Sender: TObject);
    procedure ShowSelectedObject1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ShowLinesAffected1Click(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    procedure ScriptWindow1Click(Sender: TObject);
    procedure mRecentClick(Sender: TObject);
    procedure TabsActiveTabChanged(Sender: TObject; ATab: TChromeTab);
  private
    FConnections: TServerConnections;
    FBusy: Boolean;
    FShowLinesAffected: Bool;
    FLargeMode: Boolean;
    function TestConnection(AConnStr: String): Boolean;
    procedure LoadTables(Conn: TServerConnection; Node: TTreeNode);
    procedure LoadStoredProcs(Conn: TServerConnection; Node: TTreeNode);
    function SelectedServer: TServerConnection;
    procedure RefreshServerActions;
    procedure EnableForm(const Enabled: Boolean);
    procedure LoadState;
    procedure SaveState;
    procedure AddToRecents(AConnStr: TConnectionString);
    procedure ResetSizes;
  public

  end;

var
  frmSqlExec2: TfrmSqlExec2;

implementation

{$R *.dfm}

uses
  StrUtils,
  uConnection, uDatabases,
  {$IFDEF USE_SPLASH}
  uSplash,
  {$ENDIF}
  uOutputWindow,
  uContentBase, uContentScriptExec, uContentHome;

function PromptConnection(const InitialString: TConnectionString; var NewString: TConnectionString;
  var SaveRecent: Boolean): Boolean;
var
  F: TfrmConnection;
begin
  Result:= False;
  F:= TfrmConnection.Create(nil);
  try
    F.ConnStr:= InitialString;
    if F.ShowModal = mrOK then begin
      NewString:= F.ConnStr;
      SaveRecent:= F.chkSaveRecent.Checked;
      Result:= True;
    end;
  finally
    F.Free;
  end;
end;

function FileDateTime(const FN: String): TDateTime;
var
  DT: TDateTime;
begin
  FileAge(FN, DT);
  Result:= DT;
end;

{ TfrmMain }

procedure TfrmSqlExec2.FormCreate(Sender: TObject);
var
  ProgressBarStyle: integer;
  FN: String;
begin
  pMain.Align:= alClient;
  pScript.Align:= alClient;
  pConnections.Align:= alClient;
  pSelected.Height:= 240;
  TV.Align:= alClient;

  FConnections:= TServerConnections.Create(TV);

  //Set progress bar in status bar
  Stat.Panels[4].Style := psOwnerDraw;
  Prog.Parent := Stat;
  ProgressBarStyle := GetWindowLong(Prog.Handle, GWL_EXSTYLE);
  ProgressBarStyle := ProgressBarStyle - WS_EX_STATICEDGE;
  SetWindowLong(Prog.Handle, GWL_EXSTYLE, ProgressBarStyle);

  RefreshServerActions;
  LoadState;

  ResetSizes;

  if ParamCount > 0 then begin
    FN:= ParamStr(1);
    FN:= StringReplace(FN, '"', '', [rfReplaceAll]);
    //DoOpenFile(FN);
  end;

end;

procedure TfrmSqlExec2.ResetSizes;
begin

  FLargeMode:= True;

  if FLargeMode then begin
    TB.Images:= Self.Imgs32;
    TB.ButtonWidth:= 36;
    TB.ButtonHeight:= 36;
    TB.Height:= 38;
    TV.Images:= Self.Imgs24;
    TV.Font.Size:= TV.Font.Size + 2;
    ToolBar1.Images:= Imgs24;
    ToolBar1.ButtonWidth:= 30;
    ToolBar1.ButtonHeight:= 30;
    ToolBar1.Height:= 32;
  end else begin
    TB.Images:= Self.Imgs24;
    TV.Images:= Self.Imgs16;
    ToolBar1.Images:= Imgs16;
  end;

end;

procedure TfrmSqlExec2.FormDestroy(Sender: TObject);
begin
  SaveState;
  FConnections.Clear;
  FConnections.Free;
end;

procedure TfrmSqlExec2.FormShow(Sender: TObject);
begin
  {$IFDEF USE_SPLASH}
  frmSplash.Hide;
  frmSplash.Free;
  {$ENDIF}
end;

procedure TfrmSqlExec2.Help1Click(Sender: TObject);
var
  W: TfrmOutputWindow;
begin
  W:= TfrmOutputWindow.Create(nil);
  try
    W.ShowModal;
  finally
    W.Free;
  end;
end;

procedure TfrmSqlExec2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  {$IFDEF USE_V2}
  //TODO: Check each tab for saving changes

  {$ELSE}
  if FIsEdited then begin
    case MessageDlg('Would you like to save your changes before exiting?',
      mtWarning, [mbYes,mbNo,mbCancel], 0)
    of
      mrYes: begin
        //User wishes to save
        if not DoSave then begin
          //User did not save - Cancel
          Action:= TCloseAction.caNone;
        end;
      end;
      mrNo: begin
        //User does not wish to save
        //Do nothing, let it exit
      end;
      else begin
        //User cancelled
        Action:= TCloseAction.caNone;
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TfrmSqlExec2.SaveState;
var
  R: TRegistry;
begin
  R:= TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    R.RootKey:= HKEY_CURRENT_USER;
    if R.OpenKey(REG_KEY, True) then begin
      try
        R.WriteInteger('WindowState', Integer(WindowState));
        if WindowState <> TWindowState.wsMaximized then begin
          R.WriteInteger('pLeft.Width', pLeft.Width);
          R.WriteInteger('WindowWidth', Width);
          R.WriteInteger('WindowHeight', Height);
          R.WriteInteger('WindowLeft', Left);
          R.WriteInteger('WindowTop', Top);
        end;
        R.WriteInteger('pSelected.Height', pSelected.Height);
        R.WriteBool('pSelected.Visible', pSelected.Visible);
        R.WriteBool('ShowLinesAffected', FShowLinesAffected);
      finally
        R.CloseKey;
      end;
    end else begin
      //Failed to open registry key
    end;
  finally
    R.Free;
  end;
end;

procedure TfrmSqlExec2.ScriptWindow1Click(Sender: TObject);
var
  W: TfrmContentScriptExec;
begin
  W:= TfrmContentScriptExec.Create(nil);
  try
    W.ShowModal;
  finally
    W.Free;
  end;
end;

procedure TfrmSqlExec2.LoadState;
var
  R: TRegistry;
begin
  R:= TRegistry.Create(KEY_READ);
  try
    R.RootKey:= HKEY_CURRENT_USER;
    if R.KeyExists(REG_KEY) then begin
      if R.OpenKey(REG_KEY, False) then begin
        try
          if R.ValueExists('WindowState') then begin
            if TWindowState(R.ReadInteger('WindowState')) <> wsMaximized then begin

              if R.ValueExists('WindowWidth') then
                Width:= R.ReadInteger('WindowWidth')
              else
                Width:= 1200;

              if R.ValueExists('WindowHeight') then
                Height:= R.ReadInteger('WindowHeight')
              else
                Height:= 800;

              if R.ValueExists('WindowLeft') then
                Left:= R.ReadInteger('WindowLeft')
              else
                Left:= (Screen.Width div 2) - (Width div 2);

              if R.ValueExists('WindowTop') then
                Top:= R.ReadInteger('WindowTop')
              else
                Top:= (Screen.Height div 2) - (Height div 2);

            end;
          end;

          if R.ValueExists('pSelected.Height') then
            pSelected.Height:= R.ReadInteger('pSelected.Height')
          else
            pSelected.Height:= 200;

          {
          if R.ValueExists('pMessages.Height') then
            pMessages.Height:= R.ReadInteger('pMessages.Height')
          else
            pMessages.Height:= 200;
          }

          if R.ValueExists('pLeft.Width') then
            pLeft.Width:= R.ReadInteger('pLeft.Width')
          else
            pLeft.Width:= 250;

          if R.ValueExists('pSelected.Visible') then
            pSelected.Visible:= R.ReadBool('pSelected.Visible')
          else
            pSelected.Visible:= True;

          {
          if R.ValueExists('pMessages.Visible') then
            pMessages.Visible:= R.ReadBool('pMessages.Visible')
          else
            pmessages.Visible:= True;

          if R.ValueExists('ED.Font.Color') then
            ED.Font.Name:= R.ReadString('ED.Font.Name');

          if R.ValueExists('ED.Font.Size') then
            ED.Font.Size:= R.ReadInteger('ED.Font.Size');
          }

          if R.ValueExists('ShowLinesAffected') then
            FShowLinesAffected:= R.ReadBool('ShowLinesAffected')
          else
            FShowLinesAffected:= False;

        finally
          R.CloseKey;
        end;
      end else begin
        //Failed to open registry key

      end;
    end else begin
      //Registry key does not exist

    end;
  finally
    R.Free;
  end;
end;

procedure TfrmSqlExec2.TabsActiveTabChanged(Sender: TObject; ATab: TChromeTab);
begin
  //
end;

function TfrmSqlExec2.TestConnection(AConnStr: String): Boolean;
var
  DB: TADOConnection;
begin
  Result:= False;
  DB:= TADOConnection.Create(nil);
  try
    DB.LoginPrompt:= False;
    DB.ConnectionString:= AConnStr;
    try
      DB.Connected:= True;
      Result:= True;
    except
      on E: exception do begin
        MessageDlg('Failed to connect to server: '+E.Message, mtError, [mbOK], 0);
      end;
    end;
  finally
    DB.Free;
  end;
end;

procedure TfrmSqlExec2.actServerConnectExecute(Sender: TObject);
var
  Str: TConnectionString;
  C: TServerConnection;
  Rec: Boolean;
begin
  Str:= ''; // FConnectionString;
  if PromptConnection(Str, Str, Rec) then begin
    if TestConnection(Str) then begin
      C:= FConnections.AddConnection(Str);
      //cboCurConn.Items.AddObject(Str['Data Source'], C);
      if Rec then
        AddToRecents(Str);
      TV.Select(C.Node);
      {
      if cboCurConn.ItemIndex = -1 then begin
        cboCurConn.ItemIndex:= 0;
        cboCurConnClick(nil);
      end;
      }
    end;
  end;
  RefreshServerActions;
end;

procedure TfrmSqlExec2.AddToRecents(AConnStr: TConnectionString);
var
  R: TRegistry;
begin
  R:= TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    R.RootKey:= HKEY_CURRENT_USER;
    if R.OpenKey(REG_KEY_RECENT_CONN+AConnStr['Data Source'], True) then begin
      try      
        R.WriteString('ConnStr', AConnStr);
      finally
        R.CloseKey;
      end;
    end;
  finally
    R.Free;
  end;
end;

function TfrmSqlExec2.SelectedServer: TServerConnection;
var
  N: TTreeNode;
begin
  Result:= nil;
  N:= TV.Selected;
  if Assigned(N) then begin
    case N.Level of
      0: begin
        Result:= TServerConnection(N.Data);
      end;
      1: begin
        Result:= TServerConnection(N.Parent.Data);
      end;
      2: begin
        Result:= TServerConnection(N.Parent.Parent.Data);
      end;
      3: begin
        Result:= TServerConnection(N.Parent.Parent.Parent.Data);
      end;
      4: begin
        Result:= TServerConnection(N.Parent.Parent.Parent.Parent.Data);
      end;
      5: begin
        Result:= TServerConnection(N.Parent.Parent.Parent.Parent.Parent.Data);
      end;
    end;
  end;
end;

procedure TfrmSqlExec2.ShowConnections1Click(Sender: TObject);
begin
  pConnections.Visible:= not pConnections.Visible;
end;

procedure TfrmSqlExec2.ShowLinesAffected1Click(Sender: TObject);
begin
  FShowLinesAffected:= not FShowLinesAffected;
end;

procedure TfrmSqlExec2.ShowSelectedObject1Click(Sender: TObject);
begin
  pSelected.Visible:= not pSelected.Visible;
  if pSelected.Visible then begin
    Splitter2.Top:= pConnections.Top + pConnections.Height - 2;
  end;
end;

procedure TfrmSqlExec2.SpeedButton1Click(Sender: TObject);
begin
  pSelected.Visible:= False;
end;

procedure TfrmSqlExec2.SpeedButton2Click(Sender: TObject);
begin
  pConnections.Visible:= False;
end;

procedure TfrmSqlExec2.actServerDisconnectExecute(Sender: TObject);
var
  S: TServerConnection;
begin
  S:= SelectedServer;
  if Assigned(S) then begin
    if MessageDlg('Are you sure you wish to disconnect from server?',
      mtWarning, [mbYes,mbNo], 0) = mrYes then
    begin
      //cboCurConn.Items.Delete(cboCurConn.Items.IndexOfObject(S));
      FConnections.Delete(FConnections.IndexOf(S));
    end;
  end;
  RefreshServerActions;
end;

procedure TfrmSqlExec2.cboCurConnClick(Sender: TObject);
var
  C: TServerConnection;
  X: Integer;
  D: TServerDatabase;
begin
  //Changed current connection
  {
  if cboCurConn.ItemIndex >= 0 then begin
    C:= TServerConnection(cboCurConn.Items.Objects[cboCurConn.ItemIndex]);
    cboCurDatabase.Items.BeginUpdate;
    try
      cboCurDatabase.Items.Clear;
      for X := 0 to C.DatabaseCount-1 do begin
        D:= C.Databases[X];
        cboCurDatabase.Items.AddObject(D.Name, D);
      end;
    finally
      cboCurDatabase.Items.EndUpdate;
    end;
    if cboCurDatabase.Items.Count > 0 then
      cboCurDatabase.ItemIndex:= 0;
    cboCurDatabaseClick(nil);
  end else begin
    cboCurDatabase.Items.Clear;
  end;
  }
  RefreshServerActions;
end;

procedure TfrmSqlExec2.cboCurDatabaseClick(Sender: TObject);
var
  S: TServerConnection;
begin
  {
  if cboCurDatabase.Text = '[Multiple Selected]' then begin
    actScriptBatch.Execute;
  end else begin
    S:= CurrentServer;
    if Assigned(S) then begin
      S.SelDatabases.Text:= cboCurDatabase.Text;
    end;
  end;
  }
  RefreshServerActions;
end;

procedure TfrmSqlExec2.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmSqlExec2.EnableForm(const Enabled: Boolean);
begin
  FBusy:= not Enabled;

  //ED.ReadOnly:= not Enabled;
  Self.actScriptExecute.Enabled:= Enabled;
  Self.actScriptBatch.Enabled:= Enabled;
  Self.actServerConnect.Enabled:= Enabled;
  Self.actServerDisconnect.Enabled:= Enabled;
  Self.actFileNew.Enabled:= Enabled;
  Self.actFileOpen.Enabled:= Enabled;
  Self.actFileExit.Enabled:= Enabled;
  //Self.actEditUndo.Enabled:= (Enabled and ED.CanUndo);
  Self.actEditPaste.Enabled:= Enabled;
  //Self.actEditCut.Enabled:= (Enabled and (ED.SelLength > 0));
  //Self.actEditDelete.Enabled:= (Enabled and (ED.SelLength > 0));
  //Self.actEditCopy.Enabled:= (Enabled and (ED.SelLength > 0));
  Self.actEditReplace.Enabled:= Enabled;

  if Enabled then
    Screen.Cursor:= crDefault
  else
    Screen.Cursor:= crHourglass;
  Application.ProcessMessages;
end;

procedure TfrmSqlExec2.StatDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
begin
  if Panel = StatusBar.Panels[4] then
  with Prog do begin
    Top := Rect.Top;
    Left := Rect.Left;
    Width := Rect.Right - Rect.Left - 15;
    Height := Rect.Bottom - Rect.Top;
  end;
end;

procedure TfrmSqlExec2.TVClick(Sender: TObject);
begin
  RefreshServerActions;
  //TODO: Show properties specific to selected item

end;

procedure TfrmSqlExec2.RefreshServerActions;
var
  S: TServerConnection;
begin
  S:= SelectedServer;
  actServerDisconnect.Enabled:= Assigned(S);
  {
  cboCurConn.Enabled:= Assigned(S);
  cboCurDatabase.Enabled:= Assigned(S);
  if not Assigned(S) then begin
    cboCurConn.Items.Clear;
    cboCurDatabase.Items.Clear;
  end;
  }
  //S:= CurrentServer;
  actScriptExecute.Enabled:= Assigned(S);
  actScriptBatch.Enabled:= Assigned(S);

end;

procedure TfrmSqlExec2.TVExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  R: TTreeNode;
  N: TTreeNode;
  Conn: TServerConnection;
begin
  N:= Node.getFirstChild;
  if N.Text = '' then begin
    //TODO: Delete fake node, load real data
    Node.DeleteChildren;
    R:= Node.Parent;
    case Node.StateIndex of
      1: begin
        //Expanding server node
        //(Nothing to do, already loaded)
      end;
      2: begin
        //Expanding Tables Node
        Conn:= TServerConnection(R.Data);
        LoadTables(Conn, Node);
      end;
      3: begin
        //Expanding Stored Proc Node
        Conn:= TServerConnection(R.Data);
        LoadStoredProcs(Conn, Node);
      end;
      4: begin
        //Expanding Options Node
        //Conn:= TServerConnection(R.Data);
        //LoadOptions(Conn, Node);
      end;
    end;

  end;
end;

procedure TfrmSqlExec2.View1Click(Sender: TObject);
begin
  ShowConnections1.Checked:= pConnections.Visible;
  ShowSelectedObject1.Checked:= pSelected.Visible;
  //ShowMessages1.Checked:= pMessages.Visible;
  ShowLinesAffected1.Checked:= FShowLinesAffected;
end;

procedure TfrmSqlExec2.LoadTables(Conn: TServerConnection; Node: TTreeNode);
var
  N: TTreeNode;
  Q: TADOQuery;
begin
  Q:= Conn.NewQuery;
  try
    Q.SQL.Text:= 'SELECT sobjects.name as Name FROM sysobjects sobjects WHERE sobjects.xtype = ''U'' order by Name';
    Q.Open;
    while not Q.Eof do begin
      N:= TV.Items.AddChild(Node, Q.FieldByName('Name').AsString);
      N.ImageIndex:= 23;
      N.SelectedIndex:= 23;
      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

procedure TfrmSqlExec2.mRecentClick(Sender: TObject);
var
  X: Integer;
  L: TArray<String>;
  procedure A(const FN: String);
  var
    I: TMenuItem;
  begin
    I:= TMenuItem.Create(mRecent);
    I.Caption:= ExtractFileName(FN);
    mRecent.Add(I);
  end;
begin
  mRecent.Clear;
  JumpList1.GetRecentList('SQLScriptExec', L);
  for X := 0 to Length(L)-1 do begin
    A(L[X]);
  end;
end;

procedure TfrmSqlExec2.LoadStoredProcs(Conn: TServerConnection; Node: TTreeNode);
var
  N: TTreeNode;
  Q: TADOQuery;
begin
  Q:= Conn.NewQuery;
  try
    Q.SQL.Text:= 'SELECT sobjects.name as Name FROM sysobjects sobjects WHERE sobjects.xtype = ''P'' order by Name';
    Q.Open;
    while not Q.Eof do begin
      N:= TV.Items.AddChild(Node, Q.FieldByName('Name').AsString);
      N.ImageIndex:= 81;
      N.SelectedIndex:= 81;
      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

{$IFNDEF USE_V2}

procedure TfrmSqlExec.CreateNewDoc;
begin
  if FIsEdited then begin
    case MessageDlg('Would you like to save your changes?', mtWarning, [mbYes,mbNo,mbCancel], 0) of
      mrYes: begin
        if not DoSaveAs then
          Exit;
      end;
      mrNo: begin
        //Do not save changes
      end;
      else begin
        //Abort creating new
        Exit;
      end;
    end;
  end;
  ED.Clear;
  FIsNew:= True;
  FIsEdited:= False;
  ED.ClearUndo;
  FFilename:= '';
  actFileSave.Enabled:= False;
  actFileSaveAs.Enabled:= True;
  actEditUndo.Enabled:= False;
  Stat.Panels[0].Text:= '';
  tmrEdit.Enabled:= False;
  tmrEdit.Enabled:= True;
  Caption:= 'SQL Script Executer - New File';
  FFileDateTime:= Now;
  FDTChanged:= False;
end;

procedure TfrmSqlExec.DoEdited;
begin
  FIsEdited:= True;
  actFileSave.Enabled:= True;
  actFileSaveAs.Enabled:= True;
  actEditUndo.Enabled:= True;
  Stat.Panels[0].Text:= 'Modified';
  tmrEdit.Enabled:= False;
  tmrEdit.Enabled:= True;
end;

procedure TfrmSqlExec.DoSaved;
begin
  FIsEdited:= False;
  FIsNew:= False;
  actFileSave.Enabled:= False;
  actFileSaveAs.Enabled:= True;
  actEditUndo.Enabled:= False;
  Stat.Panels[0].Text:= '';
  FFileDateTime:= FileDateTime(FFilename);
  FDTChanged:= False;
end;

function TfrmSqlExec.DoSave: Boolean;
begin
  if FIsNew then begin
    Result:= DoSaveAs;
  end else begin
    try
      ED.Lines.SaveToFile(FFilename);
      DoSaved;
      Result:= True;     
      Caption:= 'SQL Script Executer - ' + FFilename;
    except
      on E: Exception do begin
        MessageDlg('Failed to save file "'+FFilename+'"', mtError, [mbOk], 0);
        Result:= DoSaveAs;
      end;
    end;
  end;
  tmrEdit.Enabled:= False;
  tmrEdit.Enabled:= True;
end;

function TfrmSqlExec.DoSaveAs: Boolean;
begin
  Result:= False;
  dlgSave.FileName:= FFilename;
  if dlgSave.Execute then begin
    try
      ED.Lines.SaveToFile(dlgSave.FileName);
      FFilename:= dlgSave.FileName;
      DoSaved;
      Result:= True;
      JumpList1.AddToRecent(FFilename); 
      Caption:= 'SQL Script Executer - ' + FFilename;
    except
      on E: Exception do begin
        //Failed to save new file
      end;
    end;
  end;
  tmrEdit.Enabled:= False;
  tmrEdit.Enabled:= True;
end;
{$ENDIF}

procedure TfrmSqlExec2.actScriptExecuteExecute(Sender: TObject);
var
  S: TServerConnection;
  R: TSqlExecResult;
  TS: DWORD;
  TTS: DWORD;
  EC: Integer;    //Error Count - Current Database
  TEC: Integer;   //Total Error Count
  TBC: Integer;   //Total Block Count
  TDC: Integer;   //Total Selected Database Count
  RAF: Integer;   //Rows Affected - Current Database
  TRAF: Integer;  //Total Rows Affected
  X: Integer;
  {$IFNDEF USE_V2}
  function FormatPlural(const Num: Integer; const Text: String): String;
  begin
    Result:= IntToStr(Num) + ' ' + Text;
    if Num <> 1 then
      Result:= Result + 's';
  end;
  procedure ClearGrids;
  begin
    FDataGrids.Clear;
    while sbData.ControlCount > 0 do
      sbData.Controls[0].Free;
  end;
  procedure AddGrid(ADataset: TClientDataSet; ABlock: TSqlExecBlock);
  var
    F: TfrmDatasetView;
  begin
    F:= TfrmDatasetView.Create(nil);
    FDataGrids.Add(F);
    F.Parent:= sbData;
    F.Show;
    if FDataGrids.Count = 1 then begin
      F.Align:= alClient;
    end else begin
      if FDataGrids.Count = 2 then begin
        //More than one now, the first can't hog it all up.
        FDataGrids[0].Align:= alTop;
        FDataGrids[0].Height:= 250;
      end;
      F.Height:= 250;
      F.Align:= alTop;
    end;
    CloneDataset(ADataset, F.CDS);
    PostMsg('Added dataset on block '+IntToStr(ABlock.Index), [], clGreen);
    MsgPages.ActivePage:= tabData;
  end;
  procedure CheckForData(ABlock: TSqlExecBlock);
  var
    Y: Integer;
    Z: Integer;
  begin
    //Display Dataset Grids
    for Y := 0 to FSqlExec.BlockCount-1 do begin
      ABlock:= FSqlExec.Blocks[Y];
      if ABlock.DatasetCount > 0 then begin
        for Z := 0 to ABlock.DatasetCount-1 do begin
          AddGrid(ABlock.Datasets[Z], ABlock);
        end;
      end;
    end;
  end;
  procedure PerformExec(DatabaseName: String);
  var
    Y: Integer;
    B: TSqlExecBlock;
  begin
    Inc(TDC);

    //Show Status in Output Message Log
    PostMsg('');
    PostMsg('Starting Execution on Database '+DatabaseName);
    S.ChangeDatabase(DatabaseName);
    TS:= GetTickCount;

    // ----- PERFORM EXECUTION -----
    R:= FSqlExec.Execute;

    //Prepare Result Totals
    TS:= GetTickCount - TS;
    EC:= 0;
    RAF:= 0;
    for Y := 0 to FSqlExec.BlockCount-1 do begin
      if FSqlExec.Blocks[Y].Status <> TSQLExecStatus.seSuccess then
        Inc(EC);
      RAF:= RAF + FSqlExec.Blocks[Y].Affected;
    end;

    //Show Results in Output Message Log
    PostMsg('Execution on Database '+DatabaseName+' of '+IntToStr(FSqlExec.BlockCount)+
      ' Block(s) Completed in '+IntToStr(TS)+' Msec');
    if FShowLinesAffected then
      PostMsg('Rows Affected: '+IntToStr(-RAF));
    if EC > 0 then
      PostMsg(FormatPlural(EC, 'Error') + ' Reported', [fsBold], clRed);
    PostMsg('----------------------------------------------------------------');

    //Update Totals
    TEC:= TEC + EC;
    TBC:= TBC + FSqlExec.BlockCount;
    TRAF:= TRAF + RAF;

    CheckForData(B);

  end;
  {$ENDIF}
begin
  {$IFNDEF USE_V2}
  pMessages.Visible:= True;
  Splitter3.Top:= ED.Top + ED.Height - 2;
  EnableForm(False);
  MsgPages.ActivePage:= tabOutput;
  try
    OutputBox.Clear;
    ClearGrids;
    S:= CurrentServer;
    if Assigned(S) then begin
      if ED.SelLength > 0 then
        FSqlExec.SQL.Text:= ED.SelText
      else
        FSqlExec.SQL.Assign(ED.Lines);
      FSqlExec.Connection:= S.Connection;
      TEC:= 0;
      TBC:= 0;
      TDC:= 0;
      FCurBlock:= 0;
      TRAF:= 0;
      case cboCurExecMethod.ItemIndex of
        1: begin
          FSqlExec.ExecMode:= TSQLExecMode.smRecordsets;
        end;
        else begin
          FSqlExec.ExecMode:= TSQLExecMode.smExecute;
        end;
      end;
      TTS:= GetTickCount;
      if cboCurDatabase.Text = '[Multiple Selected]' then begin
        for X := 0 to S.SelDatabases.Count-1 do begin
          PerformExec(S.SelDatabases[X]);
        end;
      end else begin
        PerformExec(cboCurDatabase.Text);
      end;
      TTS:= GetTickCount - TTS;
      //Show Results in Output Message Log
      PostMsg('');
      PostMsg('Execution on Selected '+FormatPlural(TDC, 'Database') +
        ' Completed in ' + IntToStr(TTS) + ' Msec', [fsBold]);
      if FShowLinesAffected then
        PostMsg('Total Rows Affected: '+IntToStr(-TRAF));
      if TEC > 0 then
        PostMsg(FormatPlural(TEC, 'Total Error') + ' Reported', [fsBold], clRed);
      PostMsg('');
    end;
  finally
    EnableForm(True);
  end;
  {$ENDIF}
  Stat.Panels[2].Text:= 'Finished';
  RefreshServerActions;
end;

end.
