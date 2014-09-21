(*
  SQL Executer
  by Jerry Dodge

  This code uses the Mozilla Public License 1.1

  Refer to readme.txt for more information

  Project managed on Google Code:
  https://code.google.com/p/sql-executer/source/browse/

*)

unit SQLExec;

interface

uses
  Classes, SysUtils, ADODB;

const
  SE_ERR_UNKNOWN            = 1;
  SE_ERR_CONNECTION_FAIL    = 2;
  SE_ERR_INVALID_CONNECTION = 3;
  SE_ERR_PARSE              = 4;
  SE_ERR_EXECUTE            = 5;

type
  ESQLExecException = class;
  TSQLExecBlock = class;
  TSQLExecBlocks = class;
  TSQLExec = class;

  ///<summary>
  ///Current status of block execution
  ///</summary>
  TSQLExecStatus = (
    ///<summary>
    ///Block not yet executed
    ///</summary>
    sePending,

    ///<summary>
    ///Block currently executing
    ///</summary>
    seExecuting,

    ///<summary>
    ///Block successfully executed
    ///</summary>
    seSuccess,

    ///<summary>
    ///Block failed to execute
    ///</summary>
    seFail);

  ///<summary>
  ///Result of TSQLExec.Execute function
  ///</summary>
  TSQLExecResult = (
    ///<summary>
    ///Successful script execution
    ///</summary>
    srSuccess,

    ///<summary>
    ///Database connection failure
    ///</summary>
    srConnFail,

    ///<summary>
    ///Script execution failure
    ///</summary>
    srSQLFail);

  ///<summary>
  ///Different options to enable/disable for handling script execution
  ///NOTE: Not completely implemented yet
  ///</summary>
  TSQLExecOption = (
    ///<summary>
    ///Use Begin/Commit/Rollback
    ///</summary>
    soUseTransactions,

    ///<summary>
    ///Abort execution on script failure
    ///</summary>
    soAbortOnFail,

    ///<summary>
    ///Forcefully parse script on execution (ignore cache)
    ///</summary>
    soForceParse,

    ///<summary>
    ///Handle PRINT statements to output text
    ///</summary>
    soPrintOutput);

  ///<summary>
  ///Set of options for script execution
  ///</summary>
  TSQLExecOptions = set of TSQLExecOption;

  ///<summary>
  ///Executed on events such as before/after execution
  ///</summary>
  TSQLBlockEvent = procedure(Sender: TSQLExec; Block: TSQLExecBlock) of object;

  ///<summary>
  ///Executed on a PRINT statement (outputs text)
  ///NOTE: Not yet implemented
  ///</summary>
  TSQLPrintEvent = procedure(Sender: TSQLExec; Block: TSQLExecBlock; Msg: String) of object;

  ///<summary>
  ///Global exception object for component
  ///</summary>
  ESQLExecException = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor Create(const Msg: string; const ErrCode: Integer);
    property ErrorCode: Integer read FErrorCode write FErrorCode;
  end;

  ///<summary>
  ///Global exception object for component's individual blocks
  ///</summary>
  ESQLExecBlockException = class(ESQLExecException)
  private
    FBlock: TSQLExecBlock;
  public
    constructor Create(const Msg: string; const ErrCode: Integer; ABlock: TSQLExecBlock);
    property Block: TSQLExecBlock read FBlock;
  end;

  ///<summary>
  ///Encapsulates a single SQL script block to be executed
  ///This object is automatically managed by the main component.
  ///</summary>
  TSQLExecBlock = class(TObject)
  private
    FOwner: TSQLExecBlocks;
    FSQL: TStringList;
    FStatus: TSQLExecStatus;
    FLine: Integer;
    FMessage: String;
    FAffected: Integer;
    function GetSQL: TStrings;
    procedure SetSQL(const Value: TStrings);
    function GetIndex: Integer;
  public
    constructor Create(AOwner: TSQLExecBlocks);
    destructor Destroy; override;

    ///<summary>
    ///Index of block within list of blocks to be executed
    ///</summary>
    property Index: Integer read GetIndex;

    ///<summary>
    ///Current status of block execution
    ///</summary>
    property Status: TSQLExecStatus read FStatus;

    ///<summary>
    ///SQL script of individual block to be executed
    ///</summary>
    property SQL: TStrings read GetSQL write SetSQL;

    ///<summary>
    ///Starting line number of block within original script
    ///</summary>
    property Line: Integer read FLine;

    ///<summary>
    ///Total number of rows affected after execution
    ///</summary>
    property Affected: Integer read FAffected;

    ///<summary>
    ///[NOT IMPLEMENTED] Message reported back from execution
    ///</summary>
    property Message: String read FMessage;
  end;

  ///<summary>
  ///Encapsulates a list of SQL script blocks to be executed
  ///This object is automatically managed by the main component.
  ///</summary>
  TSQLExecBlocks = class(TObject)
  private
    FOwner: TSQLExec;
    FItems: TList;
    function GetItem(Index: Integer): TSQLExecBlock;
  public
    constructor Create(AOwner: TSQLExec);
    destructor Destroy; override;
    function Add: TSQLExecBlock;
    procedure Delete(const Index: Integer);
    function Count: Integer;
    function IndexOf(ABlock: TSQLExecBlock): Integer;
    procedure Clear;
    property Items[Index: Integer]: TSQLExecBlock read GetItem; default;
  end;

  ///<summary>
  ///Encapsulates a SQL Script to be executed
  ///This is the core component to instantiate.
  ///</summary>
  TSQLExec = class(TComponent)
  private
    FSQL: TStringList;
    FBlocks: TSQLExecBlocks;
    FConnection: TADOConnection;
    FOptions: TSQLExecOptions;
    FParsed: Boolean;
    FOnBlockStart: TSQLBlockEvent;
    FOnBlockFinish: TSQLBlockEvent;
    FSplitWord: String;
    FOnPrint: TSQLPrintEvent;
    function GetSQL: TStrings;
    procedure SetSQL(const Value: TStrings);
    procedure SetConnection(const Value: TADOConnection);
    procedure SQLChanged(Sender: TObject);
    procedure Invalidate;
    procedure SetSplitWord(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    ///<summary>
    ///Forcefully parses the current SQL script into blocks
    ///</summary>
    procedure ParseSQL;

    ///<summary>
    ///Performs actual execution of SQL script
    ///</summary>
    ///<returns>
    ///Result of script execution
    ///</returns>
    function Execute: TSQLExecResult;

    ///<summary>
    ///Total number of lines of script to be executed
    ///</summary>
    function LineCount: Integer;

    ///<summary>
    ///Total number of blocks to be executed
    ///Also refers to total number of GO statements
    ///</summary>
    function BlockCount: Integer;

    ///<summary>
    ///Whether or not script has been parsed
    ///</summary>
    property Parsed: Boolean read FParsed;

    ///<summary>
    ///Wraps list of blocks to be executed
    ///</summary>
    property Blocks: TSQLExecBlocks read FBlocks;
  published
    ///<summary>
    ///SQL Script to be executed
    ///</summary>
    property SQL: TStrings read GetSQL write SetSQL;

    ///<summary>
    ///ADO Connection component to use for execution
    ///</summary>
    property Connection: TADOConnection read FConnection write SetConnection;

    ///<summary>
    ///Specific options to enable/disable for execution handling
    ///</summary>
    property Options: TSQLExecOptions read FOptions write FOptions;

    ///<summary>
    ///Keyword to split into different blocks (Default "GO")
    ///</summary>
    property SplitWord: String read FSplitWord write SetSplitWord;

    ///<summary>
    ///Event triggered at the beginning of a single block execution
    ///</summary>
    property OnBlockStart: TSQLBlockEvent read FOnBlockStart write FOnBlockStart;

    ///<summary>
    ///Event triggered at the finish of a single block execution
    ///</summary>
    property OnBlockFinish: TSQLBlockEvent read FOnBlockFinish write FOnBlockFinish;

    ///<summary>
    ///[NOT IMPLEMENTED] Event triggered when a `PRINT` statement is detected
    ///</summary>
    property OnPrint: TSQLPrintEvent read FOnPrint write FOnPrint;
  end;

implementation

{ ESQLExecException }

constructor ESQLExecException.Create(const Msg: string; const ErrCode: Integer);
begin
  inherited Create(Msg);
  ErrorCode:= ErrCode;
end;

{ ESQLExecBlockException }

constructor ESQLExecBlockException.Create(const Msg: string; const ErrCode: Integer;
  ABlock: TSQLExecBlock);
begin
  inherited Create(Msg, ErrCode);
  FBlock:= ABlock;
end;

{ TSQLExecBlock }

constructor TSQLExecBlock.Create(AOwner: TSQLExecBlocks);
begin
  FOwner:= AOwner;
  FSQL:= TStringList.Create;
  FStatus:= sePending;
  FMessage:= '';
  FAffected:= 0;
end;

destructor TSQLExecBlock.Destroy;
begin
  FSQL.Free;
  inherited;
end;

function TSQLExecBlock.GetIndex: Integer;
begin
  Result:= FOwner.FItems.IndexOf(Self);
end;

function TSQLExecBlock.GetSQL: TStrings;
begin
  Result:= TStrings(FSQL);
end;

procedure TSQLExecBlock.SetSQL(const Value: TStrings);
begin
  FSQL.Assign(Value);
end;

{ TSQLExecBlocks }

constructor TSQLExecBlocks.Create(AOwner: TSQLExec);
begin
  FOwner:= AOwner;
  FItems:= TList.Create;
end;

destructor TSQLExecBlocks.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TSQLExecBlocks.Add: TSQLExecBlock;
begin
  Result:= TSQLExecBlock.Create(Self);
  FItems.Add(Result);
end;

procedure TSQLExecBlocks.Clear;
begin
  while Count > 0 do
    Delete(0);
end;

function TSQLExecBlocks.Count: Integer;
begin
  Result:= FItems.Count;
end;

procedure TSQLExecBlocks.Delete(const Index: Integer);
begin
  TSQLExecBlock(FItems[Index]).Free;
  FItems.Delete(Index);
end;

function TSQLExecBlocks.GetItem(Index: Integer): TSQLExecBlock;
begin
  Result:= TSQLExecBlock(FItems[Index]);
end;

function TSQLExecBlocks.IndexOf(ABlock: TSQLExecBlock): Integer;
begin
  Result:= FItems.IndexOf(ABlock);
end;

{ TSQLExec }

constructor TSQLExec.Create(AOwner: TComponent);
begin
  inherited;
  FSQL:= TStringList.Create;
  FSQL.OnChange:= SQLChanged;
  FBlocks:= TSQLExecBlocks.Create(Self);
  FConnection:= nil;
  FOptions:= [soUseTransactions, soAbortOnFail];
  FSplitWord:= 'go';
end;

destructor TSQLExec.Destroy;
begin
  FBlocks.Free;
  FSQL.Free;
  inherited;
end;

procedure TSQLExec.ParseSQL;
var
  I: Integer;
  Line: String;
  B: TSQLExecBlock;
  EM: String;
  Comment: Boolean;
begin
  FBlocks.Clear;
  B:= FBlocks.Add; //Add first block
  B.FLine:= 0;     //Assign the starting line # of block
  Comment:= False; //Not in comment block (yet)
  try
    for I:= 0 to FSQL.Count - 1 do begin
      Line:= FSQL[I]; //Get copy of line to string
      if (Pos('use ', LowerCase(Trim(Line))) = 1) and (not Comment) then begin //USE Statement
        //FSQL[I]:= '';     //Implement later
      end
      else if (SameText(FSplitWord, Trim(Line))) and (not Comment) then begin //GO Statement
        B:= FBlocks.Add;
        B.FLine:= I; //Assign the original starting line index of block
      end
      else if (Pos('/*', Trim(Line)) = 1) then begin //Begin comment block
        //Could be more intelligent here to find it in the middle of a line
        //However, that would also require parsing strings within script
        if (Pos('*/', Trim(Line)) = 0) then          //Check if same line ends comment block
          Comment:= True;                            //Entering comment block
      end
      else if (Pos('*/', Trim(Line)) > 0) then begin //End comment block
        //Same applies here as with begin comment blocks
        Comment:= False;                             //Leaving comment block
      end
      else begin //Normal Script
        if not Comment then
          B.SQL.Append(Line); //Add SQL script to current block
      end;
    end;
    FParsed:= True; //Flag parse completion
  except
    on e: Exception do begin
      EM:= 'Failed to parse: ' + e.Message;
      raise ESQLExecBlockException.Create(EM, SE_ERR_PARSE, B);
    end;
  end;
end;

function TSQLExec.Execute: TSQLExecResult;
var
  B: TSQLExecBlock;
  X: Integer;
  R: Integer;
  EM: String;
  procedure DoRollback;
  begin
    if soUseTransactions in FOptions then
      if soAbortOnFail in FOptions then
        FConnection.RollbackTrans;
  end;

begin
  Result:= srSuccess;
  //Parse only if changes were made or if force parse configured
  if (soForceParse in FOptions) or (not FParsed) then
    ParseSQL;
  //Begin transaction if configured
  if soUseTransactions in FOptions then
    FConnection.BeginTrans;
  try
    if not FConnection.Connected then begin
      //Attempt to connect if not already
      try
        FConnection.Connected:= True;
      except
        on e: Exception do begin
          Result:= srConnFail; //Set function connect fail result
          EM:= 'Error connecting to database: ' + e.Message;
          raise ESQLExecException.Create(EM, SE_ERR_CONNECTION_FAIL);
        end;
      end;
    end;
    for X:= 0 to FBlocks.Count - 1 do begin
      B:= FBlocks[X];                 //Get next block in list
      B.FStatus:= seExecuting;        //Set block executing status
      if Assigned(FOnBlockStart) then //Trigger block start event
        FOnBlockStart(Self, B);
      try
        //Only execute if there is text
        if Trim(B.SQL.Text) <> '' then begin
          FConnection.Execute(B.SQL.Text, R); //ACTUAL SQL EXECUTION
        end;
        B.FAffected:= R;       //Set block rows affected
        B.FStatus:= seSuccess; //Set block success status
      except
        on e: Exception do begin
          B.FStatus:= seFail; //Set block fail status
          EM:= 'Error on Line ' + IntToStr(B.Line) + ': ' + e.Message;
          B.FMessage:= EM;
          Result:= srSQLFail; //Set function failure result
          //Abort execution if configured
          if soAbortOnFail in FOptions then begin
            raise ESQLExecBlockException.Create(EM, SE_ERR_EXECUTE, B);
          end;
        end;
      end;
      if Assigned(FOnBlockFinish) then //Trigger block finish event
        FOnBlockFinish(Self, B);
    end; //of for loop
    //Commit transaction if configured
    if soUseTransactions in FOptions then
      FConnection.CommitTrans;
    Result:= srSuccess; //Everything succeeded
  except
    on e: ESQLExecBlockException do begin
      Result:= srSQLFail;              //Set function failure result
      if Assigned(FOnBlockFinish) then //Trigger block finish event
        FOnBlockFinish(Self, e.Block);
      //Rollback transaction if configured
      DoRollback;
      //raise e; //Re-raise exception
    end;
    on e: ESQLExecException do begin
      Result:= srSQLFail; //Set function failure result
      //Rollback transaction if configured
      DoRollback;
      //raise e; //Re-raise exception
    end;
    on e: Exception do begin
      Result:= srSQLFail; //Set function failure result
      //Rollback transaction if configured
      DoRollback;
      //raise ESQLExecException.Create(EM, SE_ERR_UNKNOWN);
    end;
  end;
end;

procedure TSQLExec.Invalidate;
begin
  FParsed:= False;
  FBlocks.Clear;
end;

function TSQLExec.LineCount: Integer;
begin
  Result:= FSQL.Count;
end;

function TSQLExec.BlockCount: Integer;
begin
  if not FParsed then
    ParseSQL; //Parse if not already
  Result:= FBlocks.Count;
end;

function TSQLExec.GetSQL: TStrings;
begin
  Result:= TStrings(FSQL);
end;

procedure TSQLExec.SetConnection(const Value: TADOConnection);
begin
  FConnection:= Value;
end;

procedure TSQLExec.SetSplitWord(const Value: String);
begin
  FSplitWord:= Value;
  Invalidate;
end;

procedure TSQLExec.SetSQL(const Value: TStrings);
begin
  FSQL.Assign(Value);
  Invalidate;
end;

procedure TSQLExec.SQLChanged(Sender: TObject);
begin
  Invalidate;
end;

end.