unit uMain;

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
  - Implement opening script file from outside IDE
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


*)

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.ShellApi, Winapi.ShlObj,
  System.SysUtils, System.Variants, System.Types, System.UITypes,
  System.Classes, System.Generics.Collections, System.Actions,
  System.Win.Registry,
  SQLExec, SQLExecThread,
  uDatasetView,
  DB, ADODB, DBClient, MidasLib,
  OleDB, ComObj, ActiveX,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ComCtrls,
  Vcl.ToolWin, Vcl.StdCtrls,  Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnMan, Vcl.ImgList, Vcl.ExtCtrls, Vcl.ExtDlgs, Vcl.Buttons, Vcl.Grids,

  SynEdit, SynEditHighlighter, SynHighlighterSQL,
  Vcl.JumpList,
  SynMemo,
  SynHighlighterPas, SynEditMiscClasses, SynEditSearch,
  Vcl.DBGrids,
  ChromeTabs;

const
  REG_KEY = 'Software\JD Software\SqlScriptExec\';
  REG_KEY_RECENT_CONN = 'Software\JD Software\SqlScriptExec\RecentConn\';


type
  TTreeData = class;
  TServerConnections = class;
  TServerConnection = class;
  TServerDatabase = class;
  TServerDatabaseTable = class;
  TServerDatabaseStoredProc = class;

  TTreeData = class(TObject)
  private
    FNode: TTreeNode;
  public
    constructor Create(ANode: TTreeNode);
    property Node: TTreeNode read FNode;
  end;

  TServerEvent = procedure(Sender: TObject; Server: TServerConnection) of object;

  TServerConnections = class(TObject)
  private
    FTree: TTreeView;
    FItems: TObjectList<TServerConnection>;
    FOnConnect: TServerEvent;
    FOnDisconnect: TServerEvent;
    function GetItem(Index: Integer): TServerConnection;
  public
    constructor Create(ATree: TTreeView);
    destructor Destroy; override;
    function AddConnection(AConnStr: String): TServerConnection;
    function Count: Integer;
    procedure Clear;
    procedure Delete(const Index: Integer);
    function IndexOf(AConnection: TServerConnection): Integer;
  public
    property Items[Index: Integer]: TServerConnection read GetItem; default;

    property OnConnect: TServerEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TServerEvent read FOnDisconnect write FOnDisconnect;
  end;

  TServerConnection = class(TTreeData)
  private
    FOwner: TServerConnections;
    FConnectionString: TConnectionString;
    FConnection: TADOConnection;
    FDatabases: TObjectList<TServerDatabase>;
    FDatabasesLoaded: Boolean;
    FSelDatabases: TStringList;
    procedure SetConnectionString(const Value: TConnectionString);
    function GetDatabase(const Index: Integer): TServerDatabase;
  public
    constructor Create(ANode: TTreeNode; AOwner: TServerConnections);
    destructor Destroy; override;
    function NewQuery: TADOQuery;
    property Connection: TADOConnection read FConnection;
    property ConnectionString: TConnectionString read FConnectionString write SetConnectionString;
    procedure RefreshDatabases;
    function DatabaseCount: Integer;
    procedure ChangeDatabase(const DB: String);
    property Databases[const Index: Integer]: TServerDatabase read GetDatabase;
    function SelDatabases: TStringList;
  end;

  TServerDatabase = class(TTreeData)
  private
    FOwner: TServerConnection;
    FName: String;
    //FTableNode: TTreeNode;
    //FStoredProcNode: TTreeNode;
    //FOptionsNode: TTreeNode;
    FTables: TObjectList<TServerDatabaseTable>;
    FStoredProcs: TObjectList<TServerDatabaseStoredProc>;
    function GetStoredProc(const Index: Integer): TServerDatabaseStoredProc;
    function GetTable(const Index: Integer): TServerDatabaseTable;
  public
    constructor Create(ANode: TTreeNode; AOwner: TServerConnection; const AName: String);
    destructor Destroy; override;
    procedure RefreshTables;
    procedure RefreshStoredProcs;
    function TableCount: Integer;
    function StoredProcCount: Integer;
    property Name: String read FName;
    property Tables[const Index: Integer]: TServerDatabaseTable read GetTable;
    property StoredProcs[const Index: Integer]: TServerDatabaseStoredProc read GetStoredProc;
  end;

  TServerDatabaseTable = class(TTreeData)
  private
    FOwner: TServerDatabase;
    FTableName: String;
  public
    constructor Create(AOwner: TServerDatabase; const ATableName: String);
    destructor Destroy; override;
    property TableName: String read FTableName;
  end;

  TServerDatabaseStoredProc = class(TTreeData)
  private
    FOwner: TServerDatabase;
    FProcName: String;
  public
    constructor Create(AOwner: TServerDatabase; const AProcName: String);
    destructor Destroy; override;
    property ProcName: String read FProcName;
  end;

  TfrmSqlExec = class(TForm)
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
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    ToolButton11: TToolButton;
    ToolButton13: TToolButton;
    Script1: TMenuItem;
    ExecuteScript1: TMenuItem;
    N6: TMenuItem;
    Splitter2: TSplitter;
    Disconnect1: TMenuItem;
    actServerDisconnect: TAction;
    ToolButton7: TToolButton;
    actScriptExecute: TAction;
    pScript: TPanel;
    Panel2: TPanel;
    cboCurConn: TComboBox;
    Label1: TLabel;
    cboCurDatabase: TComboBox;
    Label2: TLabel;
    ED: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    JumpList1: TJumpList;
    pMessages: TPanel;
    Splitter3: TSplitter;
    Panel3: TPanel;
    MsgPages: TPageControl;
    tabOutput: TTabSheet;
    tabSearch: TTabSheet;
    Label3: TLabel;
    tabBlocks: TTabSheet;
    cmdCloseMessages: TSpeedButton;
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
    tmrBlockCount: TTimer;
    Prog: TProgressBar;
    tmrEdit: TTimer;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    actScriptBatch: TAction;
    OutputBox: TRichEdit;
    Search: TSynEditSearch;
    actEditFindPrev: TAction;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    FindPrevious1: TMenuItem;
    ShowLinesAffected1: TMenuItem;
    tabData: TTabSheet;
    sbData: TScrollBox;
    cboCurExecMethod: TComboBox;
    Label5: TLabel;
    Tabs: TChromeTabs;
    OutputWindow1: TMenuItem;
    ScriptWindow1: TMenuItem;
    mRecent: TMenuItem;
    N2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actServerConnectExecute(Sender: TObject);
    procedure actServerDisconnectExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actEditUndoExecute(Sender: TObject);
    procedure actScriptFontExecute(Sender: TObject);
    procedure actScriptExecuteExecute(Sender: TObject);
    procedure actEditSelectAllExecute(Sender: TObject);
    procedure actScriptBatchExecute(Sender: TObject);
    procedure actEditFindExecute(Sender: TObject);
    procedure actEditFindNextExecute(Sender: TObject);
    procedure actEditFindPrevExecute(Sender: TObject);
    procedure actEditReplaceExecute(Sender: TObject);
    procedure TVClick(Sender: TObject);
    procedure TVExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure OutputBoxGutterGetText(Sender: TObject; aLine: Integer;
      var aText: string);
    procedure EDGutterGetText(Sender: TObject; aLine: Integer;
      var aText: string);
    procedure EDKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EDMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EDChange(Sender: TObject);
    procedure StatDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure tmrBlockCountTimer(Sender: TObject);
    procedure tmrEditTimer(Sender: TObject);
    procedure cboCurConnClick(Sender: TObject);
    procedure cboCurDatabaseClick(Sender: TObject);
    procedure cmdCloseMessagesClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure View1Click(Sender: TObject);
    procedure ShowConnections1Click(Sender: TObject);
    procedure ShowSelectedObject1Click(Sender: TObject);
    procedure ShowMessages1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ShowLinesAffected1Click(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    procedure ScriptWindow1Click(Sender: TObject);
    procedure mRecentClick(Sender: TObject);
  private
    FConnections: TServerConnections;
    FSqlExec: TSqlExec;
    FIsNew: Boolean;
    FIsEdited: Boolean;
    FFilename: String;
    FConnectionString: TConnectionString;
    FCurBlock: Integer;
    FBusy: Boolean;
    FShowLinesAffected: Bool;
    FLargeMode: Boolean;
    FDataGrids: TObjectList<TfrmDatasetView>;
    procedure CreateNewDoc;
    function DoSaveAs: Boolean;
    function DoSave: Boolean;
    procedure DoEdited;
    procedure DoSaved;
    procedure LoadTables(Conn: TServerConnection; Node: TTreeNode);
    function TestConnection(AConnStr: String): Boolean;
    procedure LoadStoredProcs(Conn: TServerConnection; Node: TTreeNode);
    function SelectedServer: TServerConnection;
    function CurrentServer: TServerConnection;
    procedure RefreshServerActions;
    procedure RefreshCursorPos;
    procedure BlockFinished(Sender: TSQLExec; Block: TSQLExecBlock);
    procedure BlockStarted(Sender: TSQLExec; Block: TSQLExecBlock);
    procedure SqlPrint(Sender: TSQLExec; Block: TSQLExecBlock; Msg: String);
    procedure PostMsg(const Text: String; const Style: TFontStyles = [];
      const Color: TColor = clBlack; const Detail: String = '');
    procedure EnableForm(const Enabled: Boolean);
    procedure LoadState;
    procedure SaveState;
    procedure AddToRecents(AConnStr: TConnectionString);
    procedure ResetSizes;
    function CurrentBlock: Integer;
    function TotalBlocks: Integer;
    function SelectedDatabases: Integer;
    procedure DoOpenFile(const AFilename: String);
  public

  end;

var
  frmSqlExec: TfrmSqlExec;

const
  SHARD_PIDL  = 1;
  SHARD_PATHA  = 1;
  SHARD_PATHW  = 1;
  SHARD_APPIDINFO  = 1;
  SHARD_APPIDINFOIDLIST  = 1;
  SHARD_LINK  = 1;
  SHARD_APPIDINFOLINK  = 1;
  SHARD_SHELLITEM  = 1;


procedure SHAddToRecentDocs(uFlags: UINT; const pv: LPCVOID); stdcall;
  external 'Shell32.dll';


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

{ TTreeData }

constructor TTreeData.Create(ANode: TTreeNode);
begin
  FNode:= ANode;
end;

{ TServerConnections }

constructor TServerConnections.Create(ATree: TTreeView);
begin
  FTree:= ATree;
  FItems:= TObjectList<TServerConnection>.Create(True);
end;

destructor TServerConnections.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TServerConnections.GetItem(Index: Integer): TServerConnection;
begin
  Result:= FItems[Index];
end;

function TServerConnections.IndexOf(AConnection: TServerConnection): Integer;
begin
  Result:= FItems.IndexOf(AConnection);
end;

function TServerConnections.AddConnection(
  AConnStr: String): TServerConnection;
var
  N: TTreeNode;
  //SN: TTreeNode;
  //EN: TTreeNode;
  CS: TConnectionString;
  function A(const Owner: TTreeNode; const Text: String; const Img, State: Integer): TTreeNode;
  begin
    Result:= FTree.Items.AddChild(Owner, Text);
    Result.ImageIndex:= Img;
    Result.SelectedIndex:= Img;
    Result.StateIndex:= State;
  end;
begin
  CS:= AConnStr;
  N:= A(nil, CS['Data Source'] + '  (' + CS['User Id'] + ')', 22, 1);
  Result:= TServerConnection.Create(N, Self);
  Result.FConnectionString:= AConnStr;
  Result.FConnection.ConnectionString:= AConnStr;
  FItems.Add(Result);
  N.Data:= Result;
  Result.RefreshDatabases;
  {
  SN:= A(N, 'Tables', 58, 2);
  EN:= A(SN, '', -1, 5);
  SN:= A(N, 'Stored Procedures', 78, 3);
  EN:= A(SN, '', -1, 6);
  SN:= A(N, 'Options', 17, 4);
  EN:= A(SN, '', -1, 7);
  }
  N.Expand(False);
  if Assigned(FOnConnect) then
    FOnConnect(Self, Result);
end;

procedure TServerConnections.Clear;
begin
  while Count > 0 do
    Delete(0);
end;

function TServerConnections.Count: Integer;
begin
  Result:= FItems.Count;
end;

procedure TServerConnections.Delete(const Index: Integer);
var
  C: TServerConnection;
begin
  C:= FItems[Index];
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self, C);
  C.Connection.Connected:= False;
  FTree.Items.Delete(C.Node);
  FItems.Delete(Index);
end;

{ TServerConnection }

procedure TServerConnection.ChangeDatabase(const DB: String);
begin
  FConnection.Connected:= False;
  FConnectionString['Initial Catalog']:= DB;
  FConnection.ConnectionString:= FConnectionString;
  FConnection.Connected:= True;
end;

constructor TServerConnection.Create(ANode: TTreeNode; AOwner: TServerConnections);
begin
  inherited Create(ANode);
  FOwner:= AOwner;
  FConnection:= TADOConnection.Create(nil);
  FConnection.LoginPrompt:= False;
  FDatabases:= TObjectList<TServerDatabase>.Create(True);
  FDatabasesLoaded:= False;
  FSelDatabases:= TStringList.Create;
end;

destructor TServerConnection.Destroy;
begin
  FSelDatabases.Free;
  FConnection.Connected:= False;
  FDatabases.Clear;
  FDatabases.Free;
  FConnection.Free;
  inherited;
end;

function TServerConnection.DatabaseCount: Integer;
begin
  Result:= FDatabases.Count;
end;

function TServerConnection.GetDatabase(const Index: Integer): TServerDatabase;
begin
  Result:= FDatabases[Index];
end;

function TServerConnection.NewQuery: TADOQuery;
begin
  Result:= TADOQuery.Create(nil);
  Result.Connection:= FConnection;
end;

procedure TServerConnection.RefreshDatabases;
var
  Q: TADOQuery;
  N: TTreeNode;
  D: TServerDatabase;
  S: String;
begin
  FDatabases.Clear;
  Node.DeleteChildren;
  Q:= NewQuery;
  try
    Q.SQL.Text:= 'select Name from master.dbo.sysdatabases order by Name';
    Q.Open;
    while not Q.Eof do begin
      S:= Q.FieldByName('Name').AsString;
      N:= FOwner.FTree.Items.AddChild(Node, S);
      N.ImageIndex:= 58;
      N.SelectedIndex:= 58;
      N.StateIndex:= 2;
      D:= TServerDatabase.Create(N, Self, S);
      FDatabases.Add(D);
      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

function TServerConnection.SelDatabases: TStringList;
begin
  Result:= FSelDatabases;
end;

procedure TServerConnection.SetConnectionString(const Value: TConnectionString);
begin
  FConnection.Connected:= False;
  FConnectionString := String(Value);
  FConnection.ConnectionString:= Value;
  FConnection.Connected:= True;
end;

{ TServerDatabase }

constructor TServerDatabase.Create(ANode: TTreeNode; AOwner: TServerConnection;
  const AName: String);
begin
  inherited Create(ANode);
  FOwner:= AOwner;
  FName:= AName;
  FTables:= TObjectList<TServerDatabaseTable>.Create(True);
  FStoredProcs:= TObjectList<TServerDatabaseStoredProc>.Create(True);
end;

destructor TServerDatabase.Destroy;
begin
  FStoredProcs.Free;
  FTables.Free;
  inherited;
end;

function TServerDatabase.GetStoredProc(const Index: Integer): TServerDatabaseStoredProc;
begin
  Result:= FStoredProcs[Index];
end;

function TServerDatabase.GetTable(const Index: Integer): TServerDatabaseTable;
begin
  Result:= FTables[Index];
end;

procedure TServerDatabase.RefreshStoredProcs;
var
  Q: TADOQuery;
begin
  Q:= FOwner.NewQuery;
  try
    Q.SQL.Text:= 'select * from ';
  finally
    Q.Free;
  end;
end;

procedure TServerDatabase.RefreshTables;
begin

end;

function TServerDatabase.StoredProcCount: Integer;
begin
  Result:= FStoredProcs.Count;
end;

function TServerDatabase.TableCount: Integer;
begin
  Result:= FTables.Count;
end;

{ TServerDatabaseTable }

constructor TServerDatabaseTable.Create(AOwner: TServerDatabase;
  const ATableName: String);
begin
  FOwner:= AOwner;
  FTableName:= ATableName;
end;

destructor TServerDatabaseTable.Destroy;
begin

  inherited;
end;

{ TServerDatabaseStoredProc }

constructor TServerDatabaseStoredProc.Create(AOwner: TServerDatabase;
  const AProcName: String);
begin
  FOwner:= AOwner;
  FProcName:= AProcName;
end;

destructor TServerDatabaseStoredProc.Destroy;
begin

  inherited;
end;

{ TfrmMain }

procedure TfrmSqlExec.FormCreate(Sender: TObject);
var
  ProgressBarStyle: integer;
  FN: String;
begin
  pMain.Align:= alClient;
  pScript.Align:= alClient;
  pConnections.Align:= alClient;
  TV.Align:= alClient;
  ED.Align:= alClient;
  OutputBox.Align:= alClient;
  sbData.Align:= alClient;
  //ED.Gutter.LineNumberGap:= 10;
  //WindowState:= wsMaximized;

  FConnections:= TServerConnections.Create(TV);
  FSqlExec:= TSqlExec.Create(nil);
  FSqlExec.Options:= [soUseTransactions, soPrintOutput, soForceParse];  
  FSqlExec.OnBlockStart:= BlockStarted;
  FSqlExec.OnBlockFinish:= BlockFinished;
  FSqlExec.OnPrint:= SqlPrint;
  FDataGrids:= TObjectList<TfrmDatasetView>.Create(True);

  FIsEdited:= False;
  CreateNewDoc;

  //TEMP
  FConnectionString['Provider']:= 'SQLOLEDB.1';
  FConnectionString['Persist Security Info']:= 'True';
  FConnectionString['Data Source']:= 'INNO004, 11303';
  FConnectionString['Initial Catalog']:= 'master';
  FConnectionString['User ID']:= 'sa';
  FConnectionString['Password']:= 'RMPro9a2012';

  //enable status bar 2nd Panel custom drawing
  Stat.Panels[4].Style := psOwnerDraw;
  //place the progress bar into the status bar
  Prog.Parent := Stat;
  //remove progress bar border
  ProgressBarStyle := GetWindowLong(Prog.Handle, GWL_EXSTYLE);
  ProgressBarStyle := ProgressBarStyle - WS_EX_STATICEDGE;
  SetWindowLong(Prog.Handle, GWL_EXSTYLE, ProgressBarStyle);

  RefreshCursorPos;         
  RefreshServerActions;
  LoadState;

  ResetSizes;

  if ParamCount > 0 then begin
    FN:= ParamStr(1);
    FN:= StringReplace(FN, '"', '', [rfReplaceAll]);
    DoOpenFile(FN);
  end;

end;

procedure TfrmSqlExec.ResetSizes;
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
    //ED.Font.Size:= ED.Font.Size + 2;
    OutputBox.Font.Size:= OutputBox.Font.Size + 2;
  end else begin
    TB.Images:= Self.Imgs24;
    TV.Images:= Self.Imgs16;
    ToolBar1.Images:= Imgs16;
    //ED.Font.Size:= ED.Font.Size + 4;
    //OutputBox.Font.Size:= OutputBox.Font.Size + 4;
  end;

end;

procedure TfrmSqlExec.FormDestroy(Sender: TObject);
begin
  SaveState;
  FDataGrids.Free;
  FSqlExec.Free;
  FConnections.Clear;
  FConnections.Free;
end;

procedure TfrmSqlExec.FormShow(Sender: TObject);
begin
  {$IFDEF USE_SPLASH}
  frmSplash.Hide;
  frmSplash.Free;
  {$ENDIF}
end;

procedure TfrmSqlExec.Help1Click(Sender: TObject);
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

procedure TfrmSqlExec.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Self.FIsEdited then begin
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
end;

procedure TfrmSqlExec.SaveState;
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
        R.WriteInteger('pMessages.Height', pMessages.Height);
        R.WriteBool('pSelected.Visible', pSelected.Visible);
        R.WriteBool('pMessages.Visible', pMessages.Visible);
        R.WriteString('ED.Font.Name', ED.Font.Name);
        R.WriteInteger('ED.Font.Size', ED.Font.Size);
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

procedure TfrmSqlExec.ScriptWindow1Click(Sender: TObject);
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

procedure TfrmSqlExec.LoadState;
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

          if R.ValueExists('pMessages.Height') then
            pMessages.Height:= R.ReadInteger('pMessages.Height')
          else
            pMessages.Height:= 200;

          if R.ValueExists('pLeft.Width') then
            pLeft.Width:= R.ReadInteger('pLeft.Width')
          else
            pLeft.Width:= 250;

          if R.ValueExists('pSelected.Visible') then
            pSelected.Visible:= R.ReadBool('pSelected.Visible')
          else
            pSelected.Visible:= True;

          if R.ValueExists('pMessages.Visible') then
            pMessages.Visible:= R.ReadBool('pMessages.Visible')
          else
            pmessages.Visible:= True;

          if R.ValueExists('ED.Font.Color') then
            ED.Font.Name:= R.ReadString('ED.Font.Name');

          if R.ValueExists('ED.Font.Size') then
            ED.Font.Size:= R.ReadInteger('ED.Font.Size');

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

procedure TfrmSqlExec.actEditFindExecute(Sender: TObject);
begin
  //Find text...
  Search.Pattern:= 'test';
  Search.FindFirst('test');

end;

procedure TfrmSqlExec.actEditFindNextExecute(Sender: TObject);
begin
  //Find next...
end;

procedure TfrmSqlExec.actEditFindPrevExecute(Sender: TObject);
begin
  //
end;

procedure TfrmSqlExec.actEditReplaceExecute(Sender: TObject);
begin
  //
end;

procedure TfrmSqlExec.actEditSelectAllExecute(Sender: TObject);
begin
  ED.SelectAll;
end;

procedure TfrmSqlExec.actEditUndoExecute(Sender: TObject);
begin
  ED.Undo;
  actEditUndo.Enabled:= ED.UndoList.CanUndo;
  tmrEdit.Enabled:= False;
  tmrEdit.Enabled:= True;
end;

function TfrmSqlExec.TestConnection(AConnStr: String): Boolean;
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

procedure TfrmSqlExec.tmrBlockCountTimer(Sender: TObject);
begin
  if FBusy then begin
    if not Prog.Visible then
      Prog.Visible:= True;
    if Prog.Max <> TotalBlocks then
      Prog.Max:= TotalBlocks;
    if Prog.Position <> CurrentBlock then
      Prog.Position:= CurrentBlock;
    Stat.Panels[2].Text:= IntToStr(CurrentBlock+1)+' / '+IntToStr(TotalBlocks);
  end else begin
    Prog.Visible:= False;
    Prog.Position:= 0;
  end;
end;

function TfrmSqlExec.TotalBlocks: Integer;
begin
  Result:= FSqlExec.BlockCount * SelectedDatabases;
end;

function TfrmSqlExec.CurrentBlock: Integer;
begin
  Result:= FCurBlock;
end;

function TfrmSqlExec.SelectedDatabases: Integer;
var
  S: TServerConnection;
begin
  Result:= 0;
  S:= SelectedServer;
  if Assigned(S) then begin
    Result:= S.SelDatabases.Count;
  end;
end;

procedure TfrmSqlExec.tmrEditTimer(Sender: TObject);
begin
  tmrEdit.Enabled:= False;
  FSqlExec.SQL.Assign(ED.Lines);
  Stat.Panels[3].Text:= IntToStr(FSqlExec.BlockCount)+' Blocks';
end;

procedure TfrmSqlExec.actServerConnectExecute(Sender: TObject);
var
  Str: TConnectionString;
  C: TServerConnection;
  Rec: Boolean;
begin
  Str:= ''; // FConnectionString;
  if PromptConnection(Str, Str, Rec) then begin
    if TestConnection(Str) then begin
      C:= FConnections.AddConnection(Str);
      cboCurConn.Items.AddObject(Str['Data Source'], C);
      if Rec then
        AddToRecents(Str);
      TV.Select(C.Node);
      if cboCurConn.ItemIndex = -1 then begin
        cboCurConn.ItemIndex:= 0;
        cboCurConnClick(nil);
      end;
    end;
  end;
  RefreshServerActions;
end;

procedure TfrmSqlExec.AddToRecents(AConnStr: TConnectionString);
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

function TfrmSqlExec.SelectedServer: TServerConnection;
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

procedure TfrmSqlExec.ShowConnections1Click(Sender: TObject);
begin
  pConnections.Visible:= not pConnections.Visible;
end;

procedure TfrmSqlExec.ShowLinesAffected1Click(Sender: TObject);
begin
  FShowLinesAffected:= not FShowLinesAffected;
end;

procedure TfrmSqlExec.ShowMessages1Click(Sender: TObject);
begin
  pMessages.Visible:= not pMessages.Visible;
  if pMessages.Visible then begin
    Splitter3.Top:= ED.Top + ED.Height - 2;
  end;
end;

procedure TfrmSqlExec.ShowSelectedObject1Click(Sender: TObject);
begin
  pSelected.Visible:= not pSelected.Visible;
  if pSelected.Visible then begin
    Splitter2.Top:= pConnections.Top + pConnections.Height - 2;
  end;
end;

procedure TfrmSqlExec.SpeedButton1Click(Sender: TObject);
begin
  pSelected.Visible:= False;
end;

procedure TfrmSqlExec.SpeedButton2Click(Sender: TObject);
begin
  pConnections.Visible:= False;
end;

procedure TfrmSqlExec.actServerDisconnectExecute(Sender: TObject);
var
  S: TServerConnection;
begin
  S:= SelectedServer;
  if Assigned(S) then begin
    if MessageDlg('Are you sure you wish to disconnect from server?',
      mtWarning, [mbYes,mbNo], 0) = mrYes then
    begin
      cboCurConn.Items.Delete(cboCurConn.Items.IndexOfObject(S));
      FConnections.Delete(FConnections.IndexOf(S));
    end;
  end;
  RefreshServerActions;
end;

procedure TfrmSqlExec.cboCurConnClick(Sender: TObject);
var
  C: TServerConnection;
  X: Integer;
  D: TServerDatabase;
begin
  if cboCurConn.ItemIndex >= 0 then begin
    C:= TServerConnection(cboCurConn.Items.Objects[cboCurConn.ItemIndex]);
    cboCurDatabase.Items.BeginUpdate;
    try
      cboCurDatabase.Items.Clear;
      for X := 0 to C.DatabaseCount-1 do begin
        D:= C.Databases[X];
        cboCurDatabase.Items.AddObject(D.FName, D);
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
  RefreshServerActions;
end;

procedure TfrmSqlExec.cboCurDatabaseClick(Sender: TObject);
var
  S: TServerConnection;
begin
  if cboCurDatabase.Text = '[Multiple Selected]' then begin
    actScriptBatch.Execute;
  end else begin
    S:= CurrentServer;
    if Assigned(S) then begin
      S.SelDatabases.Text:= cboCurDatabase.Text;
    end;
  end;
  RefreshServerActions;
end;

procedure TfrmSqlExec.cmdCloseMessagesClick(Sender: TObject);
begin
  pMessages.Visible:= False;
end;

procedure TfrmSqlExec.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmSqlExec.actFileNewExecute(Sender: TObject);
begin
  CreateNewDoc;
end;

procedure TfrmSqlExec.DoOpenFile(const AFilename: String);
begin
  ED.Lines.LoadFromFile(AFilename);
  FIsNew:= False;
  FIsEdited:= False;
  ED.ClearUndo;
  FFilename:= AFilename;
  JumpList1.AddToRecent(FFilename);
  Caption:= 'SQL Script Executer - ' + FFilename;
end;

procedure TfrmSqlExec.actFileOpenExecute(Sender: TObject);
  function CheckSave: Boolean;
  begin
    Result:= True;
    if FIsEdited then begin
      case MessageDlg('Would you like to save your changes?', mtWarning, [mbYes,mbNo,mbCancel], 0) of
        mrYes: begin
          if not DoSave then
            Result:= False;
        end;
        mrNo: begin
          //Do not save changes
        end;
        else begin
          //Abort creating new
          Result:= False;
        end;
      end;
    end else begin
      Result:= True;
    end;
  end;
begin
  dlgOpen.FileName:= FFilename;
  if dlgOpen.Execute then begin
    if CheckSave then begin
      try
        DoOpenFile(dlgOpen.FileName);
      except
        on E: Exception do begin
          MessageDlg('Failed to save file: '+E.Message, mtError, [mbOk], 0);
        end;
      end;
    end;
    tmrEdit.Enabled:= False;
    tmrEdit.Enabled:= True;
  end;
end;

procedure TfrmSqlExec.actFileSaveAsExecute(Sender: TObject);
begin
  DoSaveAs;
end;

procedure TfrmSqlExec.actFileSaveExecute(Sender: TObject);
begin
  DoSave;
end;

procedure TfrmSqlExec.PostMsg(const Text: String; const Style: TFontStyles = [];
  const Color: TColor = clBlack; const Detail: String = '');
var
  L: TStringList;
  X: Integer;
begin
  //Output message text
  OutputBox.Lines.BeginUpdate;
  try
    OutputBox.SelAttributes.Style:= Style;
    OutputBox.SelAttributes.Color:= Color;
    OutputBox.Lines.Add(Text);

    //Output detail text
    if Detail <> '' then begin
      L:= TStringList.Create;
      try
        L.Text:= Detail;
        for X := 0 to L.Count-1 do begin
          OutputBox.SelAttributes.Style:= Style;
          OutputBox.SelAttributes.Color:= Color;
          OutputBox.Lines.Add('  '+L[X]);
        end;
      finally
        L.Free;
      end;
    end;

    //Jump to end of output box
    if OutputBox.CanFocus then
      OutputBox.SetFocus;
    OutputBox.SelStart := OutputBox.GetTextLen;
    OutputBox.Perform(EM_SCROLLCARET, 0, 0);
    //Application.ProcessMessages;
  finally
    OutputBox.Lines.EndUpdate;
  end;
end;

procedure TfrmSqlExec.actScriptBatchExecute(Sender: TObject);
var
  S: TServerConnection;
  X: Integer;
  N: String;
begin
  S:= CurrentServer;
  if Assigned(S) then begin
    frmDatabases.LoadDatabases(S);
    if frmDatabases.ShowModal = mrOk then begin
      if frmDatabases.CheckedCount > 0 then begin
        if frmDatabases.CheckedCount = 1 then begin
          //Only one selected
          for X := 0 to frmDatabases.Lst.Items.Count-1 do begin
            if frmDatabases.Lst.Selected[X] then begin
              N:= frmDatabases.Lst.Items[X];
              S.SelDatabases.Text:= N;
              Break;
            end;
          end;
          cboCurDatabase.ItemIndex:= cboCurDatabase.Items.IndexOf(N);
        end else begin
          //Multiple selected
          S.FSelDatabases.Clear;
          for X := 0 to frmDatabases.Lst.Count-1 do begin
            N:= frmDatabases.Lst.Items[X];
            if frmDatabases.Lst.Checked[X] then begin
              S.SelDatabases.Add(N);
            end;
          end;
          if cboCurDatabase.Items.IndexOf('[Multiple Selected]') < 0 then  begin
            cboCurDatabase.Items.AddObject('[Multiple Selected]', nil);
          end;
          cboCurDatabase.ItemIndex:= cboCurDatabase.Items.IndexOf('[Multiple Selected]');
        end;
      end else begin
        //Nothing is selected
        cboCurDatabase.ItemIndex:= 0;
        S.SelDatabases.Text:= cboCurDatabase.Text;
      end;
    end;
  end;
  RefreshServerActions;
end;
                
function TfrmSqlExec.CurrentServer: TServerConnection;
begin
  Result:= nil;
  if (cboCurConn.ItemIndex >= 0) then begin
    Result:= TServerConnection(cboCurConn.Items.Objects[cboCurConn.ItemIndex]);
  end;
end;

procedure TfrmSqlExec.actScriptExecuteExecute(Sender: TObject);
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
    //S: TSplitter;
    function Split(Top: Integer): TSplitter;
    begin
      Result:= TSplitter.Create(nil);
      Result.Parent:= sbData;
      Result.Align:= alTop;
      Result.Height:= 7;
      Result.Beveled:= True;
      Result.Top:= Top;
      Result.ResizeStyle:= rsUpdate;
      Result.AutoSnap:= False;
    end;
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
        //S:= Split(FDataGrids[0].Top + FDataGrids[0].Height - 2);
      end;
      F.Height:= 250;
      F.Align:= alTop;
    end;
    //S:= Split(F.Top + F.Height - 2);

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
  procedure Perform(DatabaseName: String);
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

    //PERFORM EXECUTION
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
begin
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
          Perform(S.SelDatabases[X]);
        end;
      end else begin
        Perform(cboCurDatabase.Text);
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
  Stat.Panels[2].Text:= 'Finished';
  RefreshServerActions;
end;

procedure TfrmSqlExec.EnableForm(const Enabled: Boolean);
begin
  FBusy:= not Enabled;

  ED.ReadOnly:= not Enabled;
  Self.actScriptExecute.Enabled:= Enabled;
  Self.actScriptBatch.Enabled:= Enabled;
  Self.actServerConnect.Enabled:= Enabled;
  Self.actServerDisconnect.Enabled:= Enabled;
  Self.actFileNew.Enabled:= Enabled;
  Self.actFileOpen.Enabled:= Enabled;
  Self.actFileExit.Enabled:= Enabled;
  Self.actEditUndo.Enabled:= (Enabled and ED.CanUndo);
  Self.actEditPaste.Enabled:= Enabled;
  Self.actEditCut.Enabled:= (Enabled and (ED.SelLength > 0));
  Self.actEditDelete.Enabled:= (Enabled and (ED.SelLength > 0));
  Self.actEditCopy.Enabled:= (Enabled and (ED.SelLength > 0));
  Self.actEditReplace.Enabled:= Enabled;

  if Enabled then
    Screen.Cursor:= crDefault
  else
    Screen.Cursor:= crHourglass;
  Application.ProcessMessages;
end;

procedure TfrmSqlExec.BlockStarted(Sender: TSQLExec; Block: TSQLExecBlock);
begin
  //PostMsg(0, 'Block Execution Started', 'Block '+IntToStr(Block.Index));
  //FCurBlock:= Block.Index;
  Inc(FCurBlock);
end;

procedure TfrmSqlExec.BlockFinished(Sender: TSQLExec; Block: TSQLExecBlock);
var
  D: String;
  X: Integer;
begin
  case Block.Status of
    sePending: PostMsg('Still Pending on Block '+IntToStr(Block.Index), [], clRed);
    seExecuting: PostMsg('Still Executing on Block '+IntToStr(Block.Index), [], clRed);
    seSuccess: begin
      if Block.Message <> '' then begin
        if not ContainsText(Block.Message, 'No more results.') then begin
          D:= Block.Message;
          PostMsg('Message on Block '+IntToStr(Block.Index), [fsItalic], clGreen, D);
        end;
      end;
      if Block.Errors.Count > 0 then begin
        for X := 0 to Block.Errors.Count-1 do begin
          if not ContainsText(Block.Errors[X], 'No more results.') then begin
            D:= Block.Errors[X];
            PostMsg('Message on Block '+IntToStr(Block.Index), [fsItalic], clBlue, D);
          end;
        end;
      end;
    end;
    seFail: begin
      D:= 'SQL Script Text:' + sLineBreak + sLineBreak + Block.SQL.Text + sLineBreak;
      PostMsg('Execution Failure on Block '+IntToStr(Block.Index) + ' - "'+Block.Message+'"', [], clRed, D);
    end;
  end;
  Application.ProcessMessages;
end;

procedure TfrmSqlExec.SqlPrint(Sender: TSQLExec; Block: TSQLExecBlock; Msg: String);
begin
  PostMsg('Print on Block '+IntToStr(Block.Index)+' "'+Msg+'"', [fsItalic], clNavy);
end;

procedure TfrmSqlExec.StatDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
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

procedure TfrmSqlExec.actScriptFontExecute(Sender: TObject);
begin
  dlgFont.Font.Assign(ED.Font);
  if dlgFont.Execute then begin
    ED.Font.Assign(dlgFont.Font);
  end;
end;

procedure TfrmSqlExec.TVClick(Sender: TObject);
begin
  RefreshServerActions;
  //TODO: Show properties specific to selected item
end;

procedure TfrmSqlExec.RefreshServerActions;
var
  S: TServerConnection;
begin
  S:= SelectedServer;
  actServerDisconnect.Enabled:= Assigned(S);
  cboCurConn.Enabled:= Assigned(S);
  cboCurDatabase.Enabled:= Assigned(S);
  if not Assigned(S) then begin
    cboCurConn.Items.Clear;
    cboCurDatabase.Items.Clear;
  end;

  S:= CurrentServer;
  actScriptExecute.Enabled:= Assigned(S);
  actScriptBatch.Enabled:= Assigned(S);

end;

procedure TfrmSqlExec.TVExpanding(Sender: TObject; Node: TTreeNode;
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

procedure TfrmSqlExec.View1Click(Sender: TObject);
begin
  ShowConnections1.Checked:= pConnections.Visible;
  ShowSelectedObject1.Checked:= pSelected.Visible;
  ShowMessages1.Checked:= pMessages.Visible;
  ShowLinesAffected1.Checked:= FShowLinesAffected;
end;

procedure TfrmSqlExec.LoadTables(Conn: TServerConnection; Node: TTreeNode);
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

procedure TfrmSqlExec.mRecentClick(Sender: TObject);
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

procedure TfrmSqlExec.OutputBoxGutterGetText(Sender: TObject; aLine: Integer;
  var aText: string);
begin
  {
  if ((aLine mod 10) = 0) or (aLine = 1) or (aLine = OutputBox.CaretY) then begin
    aText:= FormatFloat('#,###,##0', aLine);
  end else begin
    if (aLine mod 5) = 0 then
      aText:= '--'
    else
      aText:= '-';
  end;
  }
end;

procedure TfrmSqlExec.LoadStoredProcs(Conn: TServerConnection; Node: TTreeNode);
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

procedure TfrmSqlExec.EDChange(Sender: TObject);
begin
  DoEdited;
end;

procedure TfrmSqlExec.EDGutterGetText(Sender: TObject; aLine: Integer; var aText: string);
begin
  if ((aLine mod 10) = 0) or (aLine = 1) or (aLine = ED.CaretY) then begin
    aText:= FormatFloat('#,###,##0', aLine);
  end else begin
    if (aLine mod 5) = 0 then
      aText:= '--'
    else
      aText:= '-';
  end;
end;

procedure TfrmSqlExec.EDKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  RefreshCursorPos;
end;

procedure TfrmSqlExec.EDMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  RefreshCursorPos;
end;

procedure TfrmSqlExec.RefreshCursorPos;
begin
  Stat.Panels[1].Text:= 'Ln '+IntToStr(ED.CaretY)+' Col '+IntToStr(ED.CaretX);
end;

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

end.
