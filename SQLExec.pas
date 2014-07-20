unit SQLExec;

interface

uses
  Windows, Classes, SysUtils, DB, ADODB,
  Dialogs;

const
  SE_ERR_NONE = 0;
  SE_ERR_UNKNOWN = 1;
  SE_ERR_CONNECTION_FAIL = 2;
  SE_ERR_INVALID_CONNECTION = 3;
  SE_ERR_PARSE = 4;
  SE_ERR_EXECUTE = 5;

type
  ESQLExecScriptException = class;
  TSQLExecBlock = class;
  TSQLExecBlocks = class;
  TSQLExec = class;


  ESQLExecScriptException = class(Exception)
  private
    FErrorCode: Integer;
    FBlock: TSQLExecBlock;
  public
    constructor Create(const Msg: string; const ErrCode: Integer;
      ABlock: TSQLExecBlock);
    property ErrorCode: Integer read FErrorCode write FErrorCode;
    property Block: TSQLExecBlock read FBlock;
  end;


  TSQLExecStatus = (sePending, seExecuting, seSuccess, seFail);
  TSQLExecResult = (srSuccess, srConnFail, srSQLFail);

  TSQLExecOption = (soUseTransactions, soAbortOnFail, soForceParse);
  TSQLExecOptions = set of TSQLExecOption;

  TSQLBlockEvent = procedure(Sender: TSQLExec; Block: TSQLExecBlock) of object;

  TSQLExecBlock = class(TObject)
  private
    FOwner: TSQLExecBlocks;
    FSQL: TStringList;
    FStatus: TSQLExecStatus;
    FLine: Integer;
    FMessage: String;
    function GetSQL: TStrings;
    procedure SetSQL(const Value: TStrings);
    function GetIndex: Integer;
  public
    constructor Create(AOwner: TSQLExecBlocks);
    destructor Destroy; override;
    property Index: Integer read GetIndex;
    property Status: TSQLExecStatus read FStatus;
    property SQL: TStrings read GetSQL write SetSQL;
    property Line: Integer read FLine;
    property Message: String read FMessage;
  end;

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
    function GetSQL: TStrings;
    procedure SetSQL(const Value: TStrings);
    procedure SetConnection(const Value: TADOConnection);
    procedure SQLChanged(Sender: TObject);
    procedure Invalidate;
    procedure SetSplitWord(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ParseSQL;
    function Execute: TSQLExecResult;
    function LineCount: Integer;
    function BlockCount: Integer;
    property Parsed: Boolean read FParsed;
    property Blocks: TSQLExecBlocks read FBlocks;
  published
    property SQL: TStrings read GetSQL write SetSQL;
    property Connection: TADOConnection read FConnection write SetConnection;
    property Options: TSQLExecOptions read FOptions write FOptions;
    property SplitWord: String read FSplitWord write SetSplitWord;
    property OnBlockStart: TSQLBlockEvent read FOnBlockStart write FOnBlockStart;
    property OnBlockFinish: TSQLBlockEvent read FOnBlockFinish write FOnBlockFinish;
  end;


implementation

{ ESQLExecScriptError }

constructor ESQLExecScriptException.Create(const Msg: string;
  const ErrCode: Integer; ABlock: TSQLExecBlock);
begin
  inherited Create(Msg);
  ErrorCode := ErrCode;
  FBlock:= ABlock;
end;

{ TSQLExecBlock }

constructor TSQLExecBlock.Create(AOwner: TSQLExecBlocks);
begin
  FOwner:= AOwner;
  FSQL:= TStringList.Create;
  FStatus:= sePending;
  FMessage:= '';
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
  FOptions:= [soUseTransactions,soAbortOnFail];
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
  X: Integer;
  S: String;
  B: TSQLExecBlock;
begin
  FBlocks.Clear;
  B:= FBlocks.Add;        //Add first block
  B.FLine:= 0;            //Assign the starting line # of block
  try
    for X := 0 to FSQL.Count - 1 do begin
      S:= FSQL[X];          //Get copy of line to string
      if Pos('use ', LowerCase(Trim(S))) = 1 then begin
        //FSQL[X]:= '';       //Temporarily disabled
      end else
      if SameText(FSplitWord, Trim(S)) then begin
        B:= FBlocks.Add;    //Add a new block
        B.FLine:= X;        //Assign the starting line # of block
      end else begin
        B.SQL.Append(S);    //Add SQL script to current block
      end;
    end;
    FParsed:= True;
  except
    on e: Exception do begin
      raise ESQLExecScriptException.Create(e.Message, SE_ERR_EXECUTE, B);
    end;
  end;
end;

function TSQLExec.Execute: TSQLExecResult;
var
  B: TSQLExecBlock;
  X: Integer;
  R: Integer;
  EM: String;
begin
  Result:= srSuccess;
  if (soForceParse in FOptions) or (not FParsed) then
    ParseSQL;
  //Begin transaction if configured
  if soUseTransactions in FOptions then
    FConnection.BeginTrans;
  try
    if not FConnection.Connected then begin
      try
        FConnection.Connected:= True;
      except
        on e: Exception do begin
          Result:= srConnFail;
          EM:= 'Error connecting to database: '+e.Message;
          raise ESQLExecScriptException.Create(EM, SE_ERR_CONNECTION_FAIL, nil);
        end;
      end;
    end;
    for X := 0 to FBlocks.Count-1 do begin
      B:= FBlocks[X];
      B.FStatus:= seExecuting;
      if Assigned(FOnBlockStart) then
        FOnBlockStart(Self, B);
      try
        if Trim(B.SQL.Text) <> '' then begin
          FConnection.Execute(B.SQL.Text);
        end;
        B.FStatus:= seSuccess;
      except
        on e: Exception do begin
          B.FStatus:= seFail;
          Result:= srSQLFail;
          if soAbortOnFail in FOptions then begin
            EM:= 'Error on Line '+IntToStr(B.Line)+': '+e.Message;
            raise ESQLExecScriptException.Create(EM, SE_ERR_EXECUTE, B);
          end;
        end;
      end;
      if Assigned(FOnBlockFinish) then
        FOnBlockFinish(Self, B);
    end; //of for loop
    //Commit transaction if configured
    if soUseTransactions in FOptions then
      FConnection.CommitTrans;
    //Everything succeeded
    Result:= srSuccess;
  except
    on e: Exception do begin
      Result:= srSQLFail;
      //Rollback transaction if configured
      if soUseTransactions in FOptions then
        if soAbortOnFail in FOptions then
          FConnection.RollbackTrans;
      raise e; //Re-raise exception
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
    ParseSQL;
  Result:= FBlocks.Count;
end;

function TSQLExec.GetSQL: TStrings;
begin
  Result:= TStrings(FSQL);
end;

procedure TSQLExec.SetConnection(const Value: TADOConnection);
begin
  FConnection := Value;
end;

procedure TSQLExec.SetSplitWord(const Value: String);
begin
  FSplitWord := Value;
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
