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
  SQLConnections,
  uContentBase, uContentScriptExec, uContentHome,

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
    actServerConnect: TAction;
    actFileExit: TAction;
    Exit1: TMenuItem;
    Undo1: TMenuItem;
    N3: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    N4: TMenuItem;
    Find1: TMenuItem;
    FindNext1: TMenuItem;
    Rreplace1: TMenuItem;
    GoTo1: TMenuItem;
    N5: TMenuItem;
    SelectAll1: TMenuItem;
    Font1: TMenuItem;
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
    pContent: TPanel;
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
    actHome: TAction;
    ToolButton6: TToolButton;
    actFileOpen: TAction;
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
    procedure ScriptWindow1Click(Sender: TObject);
    procedure mRecentClick(Sender: TObject);
    procedure TabsActiveTabChanged(Sender: TObject; ATab: TChromeTab);
    procedure actHomeExecute(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure TabsButtonCloseTabClick(Sender: TObject; ATab: TChromeTab;
      var Close: Boolean);
    procedure actFileOpenExecute(Sender: TObject);
  private
    FConnections: TServerConnections;
    FBusy: Boolean;
    FShowLinesAffected: Bool;
    FLargeMode: Boolean;

    FHome: TfrmContentHome;

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
    procedure DisplayContent(AContent: TfrmContentBase);
    procedure SetCaption(const S: String);
    procedure DoOpenFile(const Filename: String);
    procedure CheckForParams;
    procedure OpenNewConnection(const Str: TConnectionString; const Rec: Boolean);
  public

  end;

var
  frmSqlExec2: TfrmSqlExec2;

implementation

{$R *.dfm}

uses
  StrUtils,
  uConnection, uDatabases
  {$IFDEF USE_SPLASH}
  , uSplash
  {$ENDIF}
  ;

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
begin

  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:= True;
  {$ENDIF}

  pMain.Align:= alClient;
  pContent.Align:= alClient;
  pConnections.Align:= alClient;
  pSelected.Height:= 240;
  TV.Align:= alClient;

  //Set progress bar in status bar
  Stat.Panels[4].Style := psOwnerDraw;
  Prog.Parent := Stat;
  ProgressBarStyle := GetWindowLong(Prog.Handle, GWL_EXSTYLE);
  ProgressBarStyle := ProgressBarStyle - WS_EX_STATICEDGE;
  SetWindowLong(Prog.Handle, GWL_EXSTYLE, ProgressBarStyle);
  Prog.Visible:= False;

  FConnections:= TServerConnections.Create(TV);

  FHome:= TfrmContentHome.Create(nil);

  LoadState;  //Window size / position, options, etc.

  ResetSizes; //Large mode vs. Small mode

  actHome.Execute;  //Show home content tab

  CheckForParams; //Check if parameters were included

  RefreshServerActions;

end;

procedure TfrmSqlExec2.CheckForParams;
var
  FN: String;
  X: Integer;
  Str: String;
  Tmp: String;
begin
  if ParamCount > 0 then begin
    FN:= ParamStr(1);
    FN:= StringReplace(FN, '"', '', [rfReplaceAll]);
    DoOpenFile(FN);
  end;

  if FindCmdLineSwitch('s', Str, False) then begin
    //Connection String
    OpenNewConnection(Str, False);
  end else begin
    //TODO: Auto-connect to server(s) if configured

  end;

  if FindCmdLineSwitch('d', Str, False) then begin
    //Database Name(s)

  end;

  if FindCmdLineSwitch('q', Str, False) then begin
    //Quiet Mode

  end;

  if FindCmdLineSwitch('m', Str, False) then begin
    //Output Mode

  end;

  if FindCmdLineSwitch('o', Str, False) then begin
    //Output File

  end;


end;

procedure TfrmSqlExec2.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FHome);

  SaveState;
  FConnections.Clear;
  FConnections.Free;
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

procedure TfrmSqlExec2.FormShow(Sender: TObject);
begin
  {$IFDEF USE_SPLASH}
  frmSplash.Hide;
  frmSplash.Free;
  {$ENDIF}
end;

procedure TfrmSqlExec2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  {$IFDEF USE_V2}
  //TODO: Check each tab for saving changes

  {$ELSE}
  {
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
  }
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
var
  C: TfrmContentBase;
begin
  //TODO: Display corresponding tab data
  C:= TfrmContentBase(ATab.Data);
  if Assigned(C) then begin
    Self.DisplayContent(C);
  end;
end;

procedure TfrmSqlExec2.TabsButtonCloseTabClick(Sender: TObject;
  ATab: TChromeTab; var Close: Boolean);
var
  C: TfrmContentBase;
begin
  //Check content type
  C:= TfrmContentBase(ATab.Data);
  if Assigned(C) then begin
    if C is TfrmContentHome then begin
      //Do nothing, this needs to stay created
    end else
    if C is TfrmContentScriptExec then begin
      //TODO: Prompt to save changes, etc.

      C.Free;

    end;

  end;
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
  X: Integer;
  Rec: boolean;
begin
  Str:= ''; // FConnectionString;
  if PromptConnection(Str, Str, Rec) then begin
    OpenNewConnection(Str, Rec);
  end;
  RefreshServerActions;
end;

procedure TfrmSqlExec2.OpenNewConnection(const Str: TConnectionString; const Rec: Boolean);
var
  C: TServerConnection;
begin
  if TestConnection(Str) then begin
    C:= FConnections.AddConnection(Str);
    //cboCurConn.Items.AddObject(Str['Data Source'], C);
    if Rec then
      AddToRecents(Str);
    TV.Select(C.Node);
    //TODO: Broadcast change event

    {
    if cboCurConn.ItemIndex = -1 then begin
      cboCurConn.ItemIndex:= 0;
      cboCurConnClick(nil);
    end;
    }
  end;

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
  //Self.actScriptExecute.Enabled:= Enabled;
  //Self.actScriptBatch.Enabled:= Enabled;
  Self.actServerConnect.Enabled:= Enabled;
  Self.actServerDisconnect.Enabled:= Enabled;
  Self.actFileNew.Enabled:= Enabled;
  //Self.actFileOpen.Enabled:= Enabled;
  Self.actFileExit.Enabled:= Enabled;
  //Self.actEditUndo.Enabled:= (Enabled and ED.CanUndo);
  //Self.actEditPaste.Enabled:= Enabled;
  //Self.actEditCut.Enabled:= (Enabled and (ED.SelLength > 0));
  //Self.actEditDelete.Enabled:= (Enabled and (ED.SelLength > 0));
  //Self.actEditCopy.Enabled:= (Enabled and (ED.SelLength > 0));
  //Self.actEditReplace.Enabled:= Enabled;

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
  //actScriptExecute.Enabled:= Assigned(S);
  //actScriptBatch.Enabled:= Assigned(S);

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

procedure TfrmSqlExec2.actFileNewExecute(Sender: TObject);
var
  C: TfrmContentScriptExec;
  T: TChromeTab;
begin
  //New Script Window
  C:= TfrmContentScriptExec.Create(nil);
  T:= Tabs.Tabs.Add;
  T.Data:= C;
  T.Caption:= 'SQL Script';
  T.ImageIndex:= 44;
  Self.DisplayContent(C);
end;

procedure TfrmSqlExec2.actFileOpenExecute(Sender: TObject);
begin
  if dlgOpen.Execute then begin
    DoOpenFile(dlgOpen.FileName);
  end;
end;

procedure TfrmSqlExec2.DoOpenFile(const Filename: String);
var
  C: TfrmContentScriptExec;
  T: TChromeTab;
begin
  C:= TfrmContentScriptExec.Create(nil);
  T:= Tabs.Tabs.Add;
  T.Data:= C;
  T.Caption:= 'SQL Script - '+ExtractFileName(Filename);
  T.ImageIndex:= 44;
  C.ED.Lines.LoadFromFile(Filename);
  Self.DisplayContent(C);
end;

procedure TfrmSqlExec2.SetCaption(const S: String);
begin
  Self.Caption:= 'SQL Script Executer - ' + S;
end;

procedure TfrmSqlExec2.actHomeExecute(Sender: TObject);
var
  X: Integer;
  T: TChromeTab;
  C: TfrmContentBase;
  D: Boolean;
begin
  //Show Home Page
  D:= False;
  for X := 0 to Tabs.Tabs.Count-1 do begin
    T:= Tabs.Tabs[X];
    C:= TfrmContentBase(T.Data);
    if C = FHome then begin
      Tabs.ActiveTabIndex:= X;
      D:= True;
      Break;
    end;
  end;
  if not D then begin
    T:= Tabs.Tabs.Add;
    T.Data:= FHome;
    T.Caption:= 'Home';
    T.ImageIndex:= 43;
    T.Index:= 0;
  end;
  DisplayContent(FHome);
end;

procedure TfrmSqlExec2.DisplayContent(AContent: TfrmContentBase);
begin
  AContent.Parent:= pContent;
  AContent.BorderStyle:= bsNone;
  AContent.Align:= alClient;
  AContent.Show;
  AContent.BringToFront;
end;

procedure TfrmSqlExec2.actScriptExecuteExecute(Sender: TObject);
begin

  RefreshServerActions;
end;

end.
