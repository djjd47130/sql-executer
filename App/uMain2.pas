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
    - Currently in progress
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
  - Fix Save As to automatically include filename extension
  - Fix total lines affected count
  - Implement "USES" statement
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
  System.Win.Registry, System.Win.ComObj, System.IOUtils,

  Data.DB, Data.Win.ADODB, Datasnap.DBClient, MidasLib,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ComCtrls,
  Vcl.ToolWin, Vcl.StdCtrls,  Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnMan, Vcl.ImgList, Vcl.ExtCtrls, Vcl.ExtDlgs, Vcl.Buttons, Vcl.Grids,
  Vcl.JumpList, Vcl.DBGrids,

  SQLExec, SQLExecThread,
  SQLConnections,
  SQLExecCommon,
  uContentBase, uContentScriptExec, uContentHome,

  ChromeTabs,
  ChromeTabsTypes,
  ChromeTabsUtils,
  ChromeTabsControls,
  ChromeTabsThreadTimer,
  ChromeTabsClasses,

  adpMRU;

const
  REG_KEY = 'Software\JD Software\SqlScriptExec\';
  REG_KEY_RECENT_CONN = 'Software\JD Software\SqlScriptExec\RecentConn\';
  REG_KEY_AUTO_CONN = 'Software\JD Software\SqlScriptExec\AutoConn\';

  WM_REUSE_INSTANCE = WM_USER + 101;
  WM_REFRESH_RECENTS = WM_USER + 102;

type

  TSearchRecArray = array of TSearchRec;

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
    pMain: TPanel;
    dlgOpen: TOpenTextFileDialog;
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
    cmdFindPrev: TToolButton;
    cmdFindNext: TToolButton;
    cmdFindReplace: TToolButton;
    FindPrevious1: TMenuItem;
    ShowLinesAffected1: TMenuItem;
    mRecent: TMenuItem;
    N2: TMenuItem;
    tmrFileChange: TTimer;
    ToolButton12: TToolButton;
    ToolButton14: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    actHome: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actCloseScript: TAction;
    Close1: TMenuItem;
    actEditUndo: TAction;
    actScriptFont: TAction;
    actScriptExec: TAction;
    SelView: TStringGrid;
    About1: TMenuItem;
    Tabs: TChromeTabs;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actServerConnectExecute(Sender: TObject);
    procedure actServerDisconnectExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure TVExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure View1Click(Sender: TObject);
    procedure ShowConnections1Click(Sender: TObject);
    procedure ShowSelectedObject1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ShowLinesAffected1Click(Sender: TObject);
    procedure TabsActiveTabChanged(Sender: TObject; ATab: TChromeTab);
    procedure actHomeExecute(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure TabsButtonCloseTabClick(Sender: TObject; ATab: TChromeTab;
      var Close: Boolean);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actCloseScriptExecute(Sender: TObject);
    procedure actEditUndoExecute(Sender: TObject);
    procedure TabsButtonAddClick(Sender: TObject; var Handled: Boolean);
    procedure actScriptFontExecute(Sender: TObject);
    procedure actScriptExecExecute(Sender: TObject);
    procedure TVDblClick(Sender: TObject);
    procedure TVClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure TabsTabPopupMenu(Sender: TObject; const ATab: TChromeTab;
      const PopupMenu: TPopupMenu);
  private
    FConnections: TServerConnections;
    FShowLinesAffected: Bool;
    FLargeMode: Boolean;
    FAutoExec: Boolean;
    FQuietMode: Boolean;
    FOutputFile: String;
    FMsgHwnd: HWND;
    FHome: TfrmContentHome;
    FMRU: TadpMRU;
    function TestConnection(AConnStr: String): Boolean;
    procedure LoadTables(Conn: TServerConnection; Node: TTreeNode);
    procedure LoadStoredProcs(Conn: TServerConnection; Node: TTreeNode);
    function SelectedServer: TServerConnection;
    procedure LoadState;
    procedure SaveState;
    procedure AddConnToRecents(AConnStr: TConnectionString);
    procedure ResetSizes;
    procedure DisplayContent(AContent: TfrmContentBase);
    procedure CheckForParams;
    procedure RunSilent;
    procedure OpenAutoConnections;
    procedure RunVisible;
    procedure WndMethod(var Msg: TMessage);
    function CurScript: TfrmContentScriptExec;
    procedure RefreshCaption;
    procedure ShowDatabaseDetails(ANode: TTreeNode);
    procedure ShowServerDetails(ANode: TTreeNode);
    procedure ClearSelectedObject;
    procedure AddTask(const Args, Path, Caption: string);
    procedure OpenFromCmd(ACmd: String);
    procedure RecentClicked(Sender: TObject; const FileName: String);
    procedure RecentChanged(Sender: TObject);
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    function Wnd: HWND;
    procedure DoOpenConn(const Str: TConnectionString; const Rec: Boolean);
    procedure WriteToOutput(const S: String);
    procedure DoOpenFile(const Filename: String);
    procedure AddFileToRecents(const Filename: String);
    property Connections: TServerConnections read FConnections;
    procedure RefreshActions;
    class procedure CheckForInstance;
    property MRU: TadpMRU read FMRU;
  end;

var
  frmSqlExec2: TfrmSqlExec2;

implementation

{$R *.dfm}

uses
  StrUtils,
  uDataModule,
  uConnection, uDatabases, uAbout
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
begin

  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:= True;
  {$ENDIF}

  pMain.Align:= alClient;
  pContent.Align:= alClient;
  pConnections.Align:= alClient;
  pSelected.Height:= 240;
  TV.Align:= alClient;

  FMsgHwnd:= AllocateHWnd(WndMethod);
  FConnections:= TServerConnections.Create(TV);
  FHome:= TfrmContentHome.Create(nil);

  actHome.Execute;  //Show home content tab

  FMRU:= TadpMRU.Create(Self);
  FMRU.MaxItems:= 10;
  FMRU.ShowFullPath:= False;
  FMRU.ParentMenuItem:= mRecent;
  FMRU.OnClick:= RecentClicked;
  FMRU.OnChange:= RecentChanged;
  FMRU.RegistryPath:= 'Software\JD Software\SqlScriptExec\RecentFiles';

  CheckForInstance; //Check to see if another instance os running first

  LoadState;  //Window size / position, options, etc.

  FHome.RefreshRecents;

  ResetSizes; //Large mode vs. Small mode

  OpenAutoConnections; //Automatically connect to preferred servers

  CheckForParams; //Check if parameters were included

  RefreshActions;

  AddTask(' -n', ParamStr(0), 'New Script File');

end;

procedure TfrmSqlExec2.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FHome);

  SaveState;
  DeallocateHWnd(FMsgHwnd);
  FConnections.Clear;
  FConnections.Free;
  FMRU.Free;
end;

procedure TfrmSqlExec2.FormShow(Sender: TObject);
begin
  {$IFDEF USE_SPLASH}
  frmSplash.Hide;
  frmSplash.Free;
  {$ENDIF}
end;

procedure TfrmSqlExec2.FormClose(Sender: TObject; var Action: TCloseAction);
var
  X: Integer;
  C: TfrmContentBase;
  S: TfrmContentScriptExec;
begin
  for X := 0 to Tabs.Tabs.Count-1 do begin
    C:= TfrmContentBase(Tabs.Tabs[X].Data);
    if C is TfrmContentScriptExec then begin
      S:= TfrmContentScriptExec(C);
      if not S.PromptClose then begin
        Action:= TCloseAction.caNone;
        Break;
      end;
    end;
  end;
end;

procedure TfrmSqlExec2.RecentClicked(Sender: TObject; const FileName: String);
begin
  DoOpenFile(Filename);
end;

procedure TfrmSqlExec2.RecentChanged(Sender: TObject);
begin
  //Refresh recent list
  if Assigned(FHome) then
    FHome.RefreshRecents;
end;

procedure TfrmSqlExec2.AddTask(const Args: String; const Path: String; const Caption: string);
var
  I: TJumpListItem;
begin
  I:= TJumpListItem(JumpList1.TaskList.Add);
  I.Path:= Path;
  I.Arguments:= Args;
  I.FriendlyName:= Caption;
end;

procedure TfrmSqlExec2.WndMethod(var Msg: TMessage);
var
  X: Integer;
  C: TfrmContentbase;
begin
  for X := 0 to Tabs.Tabs.Count-1 do begin
    C:= TfrmContentBase(Tabs.Tabs[X].Data);
    C.WndMethod(Msg);
  end;

  {
  if Msg.Msg = MSG_CONNECTION_ADD then begin
    C:= TServerConnection(Msg.WParam);
    if Assigned(C) then begin

    end;
  end else
  if Msg.Msg = MSG_CONNECTION_DEL then begin

  end else begin
    Msg.Result := DefWindowProc(frmSqlExec2.Wnd, Msg.Msg, Msg.wParam, Msg.lParam);
  end;
  }
end;

procedure TfrmSqlExec2.RefreshCaption;
var
  S: String;
  T: TChromeTab;
  C: TfrmContentBase;
begin
  S:= 'SQL Script Executer';
  T:= Tabs.ActiveTab;
  if Assigned(T) then begin
    C:= TfrmContentBase(T.Data);
    if Assigned(C) then begin
      S:= S + ' - ' + C.Caption;
    end;
  end;
  Caption:= S;
end;

procedure TfrmSqlExec2.WndProc(var Message: TMessage);
var
  S: PChar;
begin
  case Message.Msg of
    WM_REUSE_INSTANCE: begin
      S:= PChar(Message.WParam);
      OpenFromCmd(S);
    end;
    WM_REFRESH_RECENTS: begin

    end;
  end;
  inherited;
end;

type
  TFNWndEnumProc = function(hwnd: HWND; lParam: LPARAM): BOOL; stdcall;

function EnumWindows(lpEnumFunc: TFNWndEnumProc; lParam: LPARAM): BOOL;
  stdcall; external  user32;

function GetWindows(Handle: HWND; Info: LPARAM): BOOL; stdcall;
var
  L: TList;
begin
  Result:= True;
  L:= TList(Info);
  L.Add(Pointer(Handle));
end;

class procedure TfrmSqlExec2.CheckForInstance;
{$IFDEF USE_V3}
var
  L: TList;
  H: HWND;
  Dest: array[0..80] of char;
  I: Integer;
  S: String;
{$ENDIF}
begin

  {$IFDEF USE_V3}
  if CreateMutex(nil, True, '2F23F63E-FADE-4F67-8C50-CF72354C17BB') = 0 then
    RaiseLastOSError;

  if GetLastError = ERROR_ALREADY_EXISTS then begin
    L:= TList.Create;
    try
      EnumWindows(GetWindows, LPARAM(L));
      for I := 0 to L.Count-1 do begin
        GetWindowText(HWND(L[I]), Dest, sizeof(Dest) - 1);
        S:= Dest;
        if ContainsText(S, 'SQL Script Executer') then begin
          H:= HWND(L[I]);
          if IsWindow(H) then begin
            SendMessage(H, WM_REUSE_INSTANCE, NativeUInt(@GetCommandLine), 0);
          end;
        end;
      end
    finally
      L.Free;
    end;
    Application.Terminate;
  end;
  {$ENDIF}

end;

procedure TfrmSqlExec2.OpenFromCmd(ACmd: String);
var
  Str: String;
  Tmp: String;
  ExeName: String;
  FileName: String;
  Cmd: String;
  Val: String;
  Par: TStringList;
  P: Integer;
  CS: TfrmContentScriptExec;
  function Exists(const N: String): Boolean;
  var
    X: Integer;
  begin
    Result:= False;
    for X := 0 to Par.Count-1 do begin
      if SameText(Par.Names[X], N) then begin
        Result:= True;
        Break;
      end;
    end;
  end;
begin
  //Feed in a command line to open either new or existing file(s)
  // and detect parameters for control

  Par:= TStringList.Create;
  try
    Str:= ACmd + ' ';

    Str:= Trim(Str);

    P:= Pos('"', Str);
    if P = 1 then begin
      Delete(Str, 1, 1);
      P:= Pos('"', Str);
      Tmp:= Copy(Str, 1, P-1);
      Delete(Str, 1, P);
      ExeName:= Tmp;
    end else begin
      P:= Pos(' ', Str);
      Tmp:= Copy(Str, 1, P-1);
      Delete(Str, 1, P);
      ExeName:= Tmp;
    end;

    Str:= Trim(Str);

    P:= Pos('"', Str);
    if P = 1 then begin
      Delete(Str, 1, 1);
      P:= Pos('"', Str);
      Tmp:= Copy(Str, 1, P-1);
      Delete(Str, 1, P);
      FileName:= Tmp;
    end else begin
      {
      P:= Pos(' ', Str);
      Tmp:= Copy(Str, 1, P-1);
      Delete(Str, 1, P);
      FileName:= Tmp;
      }
    end;

    Str:= Trim(Str)+' ';

    while Length(Trim(Str)) > 0 do begin
      P:= Pos('-', Str);
      if P < 1 then
        P:= Pos('/', 'Str');
      if P > 0 then begin
        Delete(Str, 1, 1);
        P:= Pos(' ', Str);
        Tmp:= Trim(Copy(Str, 1, P-1));
        Delete(Str, 1, P);
        if Pos('"', Tmp) = 1 then begin
          Delete(Tmp, 1, 1);
          P:= Pos('"', Tmp);
          if P > 0 then
            Delete(Tmp, 1, 1);
        end;
        Cmd:= Tmp;
        Str:= Trim(Str) + ' ';
        if (Pos('-', Str) <> 1) and  (Pos('/', Str) <> 1) then begin
          P:= Pos('"', Str);
          if P = 1 then begin
            Delete(Str, 1, 1);
            P:= Pos('"', Str);
            Tmp:= Copy(Str, 1, P-1);
            Delete(Str, 1, P);
          end else begin
            P:= Pos(' ', Str);
            Tmp:= Copy(Str, 1, P-1);
            Delete(Str, 1, P);
          end;
          Val:= Tmp;
        end else begin
          Val:= '';
        end;
        if Val = '' then
          Val:= ' ';
        Par.Values[Cmd]:= Val;
      end else begin
        MessageDlg('Command line parameters malformed ('+Str+')', mtError, [mbOK], 0);
        Str:= '';
      end;
      Str:= Trim(Str) + ' ';
    end;

    //Actual check for further parameters

    if Exists('n') then begin
      actFileNew.Execute;
    end;

    if Exists('s') then begin
      Str:= Par.Values['s'];
      DoOpenConn(Str, False);
    end;

    CS:= CurScript;

    if Exists('d') then begin
      Str:= Par.Values['d'];
      if Str <> '' then begin
        if Assigned(CS) then begin
          if CS.cboCurDatabase.Items.IndexOf(Str) >= 0 then begin
            CS.cboCurDatabase.ItemIndex:= CS.cboCurDatabase.Items.IndexOf(Str);
            CS.cboCurDatabaseClick(nil);
            //TODO: Support multiple databases
          end;
        end;
      end;
    end;

    if Exists('m') then begin
      Str:= Par.Values['m'];
      if Str <> '' then begin
        if Assigned(CS) then begin
          if SameText(Str, 'data') then
            CS.cboCurExecMethod.ItemIndex:= 1
          else
            CS.cboCurExecMethod.ItemIndex:= 0;
        end;
      end;
    end;

    if Exists('e') then begin
      FAutoExec:= True;
    end;

    if Exists('w') then begin
      Str:= Par.Values['w'];
      if Str <> '' then begin
        if Assigned(CS) then begin
          CS.txtSplitWord.Text:= Str;
        end;
      end;
    end;

    if Exists('q') then begin
      Str:= Par.Values['q'];
      FQuietMode:= True;
    end;

    if Exists('o') then begin
      Str:= Par.Values['o'];
      FOutputFile:= Str;
    end else begin
      FOutputFile:= '';
    end;


  finally
    Par.Free;
  end;

  //TODO: Check for filename to open

end;

procedure TfrmSqlExec2.CheckForParams;
var
  FN: String;
begin
  if ParamCount > 0 then begin
    FN:= ParamStr(1);
    FN:= StringReplace(FN, '"', '', [rfReplaceAll]);
    if FileExists(FN) then
      DoOpenFile(FN);
  end;

  OpenFromCmd(GetCommandLine);

  {
  if FindCmdLineSwitch('n', Str, False) then begin
    //New Script
    actFileNew.Execute;
  end;

  if FindCmdLineSwitch('s', Str, False) then begin
    //Connection String
    OpenNewConnection(Str, False);
  end;

  if FindCmdLineSwitch('d', Str, False) then begin
    //Database Name(s)
    if Str <> '' then begin
      if Assigned(CS) then begin
        if CS.cboCurDatabase.Items.IndexOf(Str) >= 0 then begin
          CS.cboCurDatabase.ItemIndex:= CS.cboCurDatabase.Items.IndexOf(Str);
          CS.cboCurDatabaseClick(nil);
          //TODO: Support multiple databases
        end;
      end;
    end;
  end;

  if FindCmdLineSwitch('m', Str, False) then begin
    //Output Mode
    if Str <> '' then begin
      if Assigned(CS) then begin
        if SameText(Str, 'data') then
          CS.cboCurExecMethod.ItemIndex:= 1
        else
          CS.cboCurExecMethod.ItemIndex:= 0;
      end;
    end;
  end;

  if FindCmdLineSwitch('e', Str, False) then begin
    //Perform Execution Automatically
    FAutoExec:= True;
  end;

  if FindCmdLineSwitch('w', Str, False) then begin
    //Split Word
    if Str <> '' then begin
      if Assigned(CS) then begin
        CS.txtSplitWord.Text:= Str;
      end;
    end;
  end;

  if FindCmdLineSwitch('q', Str, False) then begin
    //Quiet Mode
    // - Only works when "e" is provided
    FQuietMode:= True;
  end;

  if FindCmdLineSwitch('o', Str, False) then begin
    //Output File
    // - Only works when "e" is provided
    FOutputFile:= Str;
  end else begin
    FOutputFile:= '';
  end;
  }

  if FAutoExec then begin
    if FQuietMode then begin
      RunSilent;
    end else begin
      RunVisible;
    end;
  end else begin

  end;

end;

function TfrmSqlExec2.CurScript: TfrmContentScriptExec;
var
  T: TChromeTab;
  C: TfrmContentBase;
begin
  Result:= nil;
  T:= Tabs.ActiveTab;
  if Assigned(T) then begin
    C:= TfrmContentBase(T.Data);
    if C is TfrmContentScriptExec then begin
      Result:= TfrmContentScriptExec(C);
    end;
  end;
end;

procedure TfrmSqlExec2.RunSilent;
begin
  Application.ShowMainForm:= False;
  Application.MainFormOnTaskBar:= True;
  //TODO: Execute

  Application.Terminate;
end;

procedure TfrmSqlExec2.RunVisible;
begin
  //TODO: Execute

end;

procedure TfrmSqlExec2.OpenAutoConnections;
var
  R: TRegistry;
  L: TStringList;
  X: Integer;
begin
  R:= TRegistry.Create(KEY_READ);
  try
    R.RootKey:= HKEY_CURRENT_USER;
    if R.KeyExists(REG_KEY_AUTO_CONN) then begin
      if R.OpenKey(REG_KEY_AUTO_CONN, False) then begin
        try
          L:= TStringList.Create;
          try
            R.GetKeyNames(L);
            for X := 0 to L.Count-1 do begin

            end;
          finally
            L.Free;
          end;
        finally
          R.CloseKey;
        end;
      end;
    end;
  finally
    R.Free;
  end;
end;

procedure TfrmSqlExec2.ResetSizes;
begin

  FLargeMode:= True;

  if FLargeMode then begin
    TB.Images:= dmDataModule.Imgs32;
    TB.ButtonWidth:= 36;
    TB.ButtonHeight:= 36;
    TB.Height:= 38;
    TV.Images:= dmDataModule.Imgs24;
    TV.Font.Size:= TV.Font.Size + 2;
    ToolBar1.Images:= dmDataModule.Imgs24;
    ToolBar1.ButtonWidth:= 30;
    ToolBar1.ButtonHeight:= 30;
    ToolBar1.Height:= 32;
  end else begin
    TB.Images:= dmDataModule.Imgs24;
    TV.Images:= dmDataModule.Imgs16;
    ToolBar1.Images:= dmDataModule.Imgs16;
  end;

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
  RefreshActions;
end;

procedure TfrmSqlExec2.TabsButtonAddClick(Sender: TObject;
  var Handled: Boolean);
begin
  Handled:= True;
  Self.actFileNew.Execute;
end;

procedure TfrmSqlExec2.TabsButtonCloseTabClick(Sender: TObject;
  ATab: TChromeTab; var Close: Boolean);
var
  C: TfrmContentBase;
  S: TfrmContentScriptExec;
begin
  //Check content type
  C:= TfrmContentBase(ATab.Data);
  if Assigned(C) then begin
    //C.Hide;
    if C is TfrmContentHome then begin
      //Nothing...
      Close:= False;
    end else
    if C is TfrmContentScriptExec then begin
      S:= TfrmContentScriptExec(C);
      if S.PromptClose then begin
        C.Free;
      end else begin
        Close:= False;
      end;
    end;
  end;
end;

procedure TfrmSqlExec2.TabsTabPopupMenu(Sender: TObject; const ATab: TChromeTab;
  const PopupMenu: TPopupMenu);
var
  F: TfrmContentBase;
  I: Integer;
begin
  if Assigned(ATab) then begin
    F:= TfrmContentBase(ATab.Data);
    if F is TfrmContentHome then begin
      for I := 0 to PopupMenu.Items.Count-1 do begin
        if PopupMenu.Items[I].Tag = 2 then begin
          PopupMenu.Items[I].Visible:= False;
          Break;
        end;
      end;
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
  Rec: boolean;
begin
  Str:= ''; // FConnectionString;
  if PromptConnection(Str, Str, Rec) then begin
    DoOpenConn(Str, Rec);
  end;
  RefreshActions;
end;

procedure TfrmSqlExec2.DoOpenConn(const Str: TConnectionString; const Rec: Boolean);
var
  C: TServerConnection;
begin
  if TestConnection(Str) then begin
    C:= FConnections.AddConnection(Str);
    if Rec then
      AddConnToRecents(Str);
    TV.Select(C.Node);
    TVClick(nil);
    DoConnectionAdded(C);
  end;
end;

procedure TfrmSqlExec2.AddConnToRecents(AConnStr: TConnectionString);
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
      DoConnectionDeleted(S);
      FConnections.Delete(FConnections.IndexOf(S));
    end;
  end;
  RefreshActions;
end;

procedure TfrmSqlExec2.About1Click(Sender: TObject);
var
  F: TfrmAbout;
begin
  F:= TfrmAbout.Create(nil);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TfrmSqlExec2.actCloseScriptExecute(Sender: TObject);
begin
  //Close currently active script


end;

procedure TfrmSqlExec2.actEditUndoExecute(Sender: TObject);
var
  S: TfrmContentScriptExec;
begin
  //TODO: Undo
  S:= CurScript;
  if Assigned(S) then begin
    S.actUndo.Execute;
  end;
end;

procedure TfrmSqlExec2.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmSqlExec2.TVClick(Sender: TObject);
var
  N: TTreeNode;
begin
  Screen.Cursor:= crHourglass;
  try
    ClearSelectedObject;
    N:= TV.Selected;
    if Assigned(N) then begin
      case N.Level of
        0: begin
          //Server Connection
          lblSelectedObject.Caption:= 'Server: ' + N.Text;
          ShowServerDetails(N);
        end;
        1: begin
          //Database
          lblSelectedObject.Caption:= 'Database: ' + N.Text;
          ShowDatabaseDetails(N);
        end;
      end;
    end else begin
      lblSelectedObject.Caption:= 'Selected Object';

    end;
  finally
    Screen.Cursor:= crDefault;
  end;
  RefreshActions;
end;

procedure TfrmSqlExec2.ClearSelectedObject;
begin
  SelView.RowCount:= 2;
  SelView.Rows[1].Clear;
  SelView.Cells[0, 0]:= 'Name';
  SelView.Cells[1, 0]:= 'Value';
end;

procedure TfrmSqlExec2.ShowServerDetails(ANode: TTreeNode);
var
  I: Integer;
  S: TServerConnection;
  procedure A(const N, S: String);
  begin
    if I > 1 then
      SelView.RowCount:= SelView.RowCount + 1;
    SelView.Cells[0, I]:= N;
    SelView.Cells[1, I]:= S;
    Inc(I);
  end;
begin
  I:= 1;
  S:= TServerConnection(ANode.Data);
  A('Server Name', S.ConnectionString['Data Source']);
  A('Current User', S.ConnectionString['User ID']);
  A('Databases', IntToStr(S.DatabaseCount));
  //A('Selected', IntToStr(S.SelDatabases.Count));
end;

procedure TfrmSqlExec2.ShowDatabaseDetails(ANode: TTreeNode);
var
  I: Integer;
  D: TServerDatabase;
  procedure A(const N, S: String);
  begin
    if I > 1 then
      SelView.RowCount:= SelView.RowCount + 1;
    SelView.Cells[0, I]:= N;
    SelView.Cells[1, I]:= S;
    Inc(I);
  end;
begin
  I:= 1;
  D:= TServerDatabase(ANode.Data);
  A('Database Name', D.Name);
  A('Table Count', IntToStr(D.TableCount));
  A('Stored Proc Count', IntToStr(D.StoredProcCount));

end;

procedure TfrmSqlExec2.TVDblClick(Sender: TObject);
var
  N: TTreeNode;
begin
  //Double-clicked node in tree view
  N:= TV.Selected;
  if Assigned(N) then begin
    pSelected.Visible:= True;
    Splitter2.Top:= pConnections.Top + pConnections.Height - 2;
  end;
end;

procedure TfrmSqlExec2.RefreshActions;
var
  Svr: TServerConnection;
  S: TfrmContentScriptExec;
begin
  Svr:= SelectedServer;
  actServerDisconnect.Enabled:= Assigned(Svr);
  S:= CurScript;
  if Assigned(S) then begin
    actFileSave.Enabled:= S.actSave.Enabled;
    actFileSaveAs.Enabled:= S.actSaveAs.Enabled;
    actEditUndo.Enabled:= S.actUndo.Enabled;
    actCloseScript.Enabled:= True;
    actScriptFont.Enabled:= S.actFont.Enabled;
    actScriptExec.Enabled:= S.actExecSql.Enabled;
  end else begin
    actFileSave.Enabled:= False;
    actFileSaveAs.Enabled:= False;
    actCloseScript.Enabled:= False;
    actEditUndo.Enabled:= False;
    actCloseScript.Enabled:= False;
    actScriptFont.Enabled:= False;
    actScriptExec.Enabled:= False;
  end;
  RefreshCaption;
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

function TfrmSqlExec2.Wnd: HWND;
begin
  Result:= Self.FMsgHwnd;
end;

procedure TfrmSqlExec2.WriteToOutput(const S: String);
begin
  if FOutputFile <> '' then begin
    if DirectoryExists(ExtractFilePath(FOutputFile)) then begin
      //TODO: Write to output file

    end else begin
      //Directory doesn't exist

    end;
  end else begin
    //Print to output window

  end;
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
  C.Tab:= T;
  T.ImageIndex:= 44;
  Self.DisplayContent(C);
end;

procedure TfrmSqlExec2.actFileOpenExecute(Sender: TObject);
begin
  if dlgOpen.Execute then begin
    DoOpenFile(dlgOpen.FileName);
  end;
end;

procedure TfrmSqlExec2.actFileSaveAsExecute(Sender: TObject);
var
  C: TfrmContentScriptExec;
begin
  C:= CurScript;
  if Assigned(C) then begin
    C.actSaveAs.Execute;
  end;
end;

procedure TfrmSqlExec2.actFileSaveExecute(Sender: TObject);
var
  C: TfrmContentScriptExec;
begin
  C:= CurScript;
  if Assigned(C) then begin
    C.actSave.Execute;
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
  T.ImageIndex:= 44;
  DisplayContent(C);
  C.Tab:= T;
  C.LoadFromFile(Filename);
  AddFileToRecents(Filename);
end;

procedure TfrmSqlExec2.AddFileToRecents(const Filename: String);
begin
  JumpList1.AddToRecent(Filename);
  FMRU.AddItem(Filename);
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
    T.Pinned:= True;
    T.HideCloseButton:= True;
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

procedure TfrmSqlExec2.actScriptExecExecute(Sender: TObject);
var
  S: TfrmContentScriptExec;
begin
  S:= CurScript;
  if Assigned(S) then begin
    S.actExecSql.Execute;
  end;
end;

procedure TfrmSqlExec2.actScriptFontExecute(Sender: TObject);
var
  S: TfrmContentScriptExec;
begin
  S:= CurScript;
  if Assigned(S) then begin
    S.actFont.Execute;
  end;
end;

end.