unit uConnection;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Types,
  System.UITypes, System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons,
  DB, ADODB, ActiveX, ComObj, AdoInt, OleDB,
  RMP.API.Common,
  Registry;

type
  TRecentConn = class(TObject)
    ConnStr: TConnectionString;
    function Caption: String;
  end;

  TfrmConnection = class(TForm)
    cboServer: TComboBox;
    Label1: TLabel;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    txtUsername: TEdit;
    Label3: TLabel;
    txtPassword: TEdit;
    Label4: TLabel;
    chkSaveRecent: TCheckBox;
    cmdDeleteRecent: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cboServerClick(Sender: TObject);
    procedure cboServerChange(Sender: TObject);
    procedure txtUsernameChange(Sender: TObject);
    procedure cmdDeleteRecentClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FRecents: TObjectList<TRecentConn>;
    procedure SetConnStr(const Value: TConnectionString);
    function GetConnStr: TConnectionString;
    function AddRecent(const AConnStr: TConnectionString): TRecentConn;
    procedure ClearRecents;
  public
    property ConnStr: TConnectionString read GetConnStr write SetConnStr;
    procedure RefreshRecents;
  end;

var
  frmConnection: TfrmConnection;

implementation

{$R *.dfm}

uses
  uMain;

procedure ListAvailableSQLServers(Names : TStrings);
var
  RSCon: ADORecordsetConstruction;
  Rowset: IRowset;
  SourcesRowset: ISourcesRowset;
  SourcesRecordset: _Recordset;
  SourcesName, SourcesType: TField;

    function PtCreateADOObject
             (const ClassID: TGUID): IUnknown;
    var
      Status: HResult;
      FPUControlWord: Word;
    begin
      asm
        FNSTCW FPUControlWord
      end;
      Status := CoCreateInstance(
                  CLASS_Recordset,
                  nil,
                  CLSCTX_INPROC_SERVER or
                  CLSCTX_LOCAL_SERVER,
                  IUnknown,
                  Result);
      asm
        FNCLEX
        FLDCW FPUControlWord
      end;
      OleCheck(Status);
    end;
begin
  SourcesRecordset :=
      PtCreateADOObject(CLASS_Recordset)
      as _Recordset;
  RSCon :=
      SourcesRecordset
      as ADORecordsetConstruction;
  SourcesRowset :=
      CreateComObject(ProgIDToClassID('SQLOLEDB Enumerator'))
      as ISourcesRowset;
  OleCheck(SourcesRowset.GetSourcesRowset(
       nil,
       IRowset, 0,
       nil,
       IUnknown(Rowset)));
  RSCon.Rowset := RowSet;
  with TADODataSet.Create(nil) do
  try
    Recordset := SourcesRecordset;
    SourcesName := FieldByName('SOURCES_NAME');
    SourcesType := FieldByName('SOURCES_TYPE');
    Names.BeginUpdate;
    try
      while not EOF do begin
        if (SourcesType.AsInteger = DBSOURCETYPE_DATASOURCE)
          and (SourcesName.AsString <> '') then
        begin
          Names.Add(SourcesName.AsString);
        end;
        Next;
      end;
    finally
      Names.EndUpdate;
    end;
  finally
    Free;
  end;
end;

{ TRecentConn }

function TRecentConn.Caption: String;
begin
  Result:= ConnStr['Data Source'];
end;

{ TfrmConnection }

procedure TfrmConnection.cboServerChange(Sender: TObject);
var
  X: Integer;
  R: TRecentConn;
begin
  BitBtn1.Enabled:= (cboServer.Text <> '') and (txtUsername.Text <> '');
  cmdDeleteRecent.Enabled:= False;
  for X := 0 to FRecents.Count-1 do begin
    R:= FRecents[X];
    if SameText(R.Caption, cboServer.Text) then begin
      cmdDeleteRecent.Enabled:= True;
      Break;
    end;
  end;
end;

procedure TfrmConnection.cboServerClick(Sender: TObject);
var
  R: TRecentConn;
begin
  if cboServer.ItemIndex < 0 then Exit;
  R:= TRecentConn(cboServer.Items.Objects[cboServer.ItemIndex]);
  if Assigned(R) then begin
    txtUsername.Text:= R.ConnStr['User Id'];
    txtPassword.Text:= R.ConnStr['Password'];
  end;
end;

procedure TfrmConnection.cmdDeleteRecentClick(Sender: TObject);
var
  R: TRegistry;
begin
  if MessageDlg('Delete recent server connection?',
    mtWarning, [mbYes,mbNo], 0) = mrYes then
  begin
    R:= TRegistry.Create(KEY_ALL_ACCESS);
    try
      R.RootKey:= HKEY_CURRENT_USER;
      if R.KeyExists(REG_KEY_RECENT_CONN+cboServer.Text) then begin
        R.DeleteKey(REG_KEY_RECENT_CONN+cboServer.Text)
      end;
    finally
      R.Free;
    end;
    RefreshRecents;
    cboServer.Text:= '';
    txtUsername.Text:= '';
    txtPassword.Text:= '';
  end;
end;

function TfrmConnection.AddRecent(const AConnStr: TConnectionString): TRecentConn;
begin
  Result:= TRecentConn.Create;
  Result.ConnStr:= AConnStr;
  FRecents.Add(Result);
end;

procedure TfrmConnection.FormCreate(Sender: TObject);
begin
  FRecents:= TObjectList<TRecentConn>.Create(True);
end;

procedure TfrmConnection.FormDestroy(Sender: TObject);
begin
  ClearRecents;
  FRecents.Free;
end;

procedure TfrmConnection.FormShow(Sender: TObject);
begin
  RefreshRecents;
  cboServer.ItemIndex:= -1;
end;

function TfrmConnection.GetConnStr: TConnectionString;
begin
  Result.Clear;
  Result['Provider']:= 'SQLOLEDB.1';
  Result['Persist Security Info']:= 'True';
  Result['Data Source']:= cboServer.Text;
  Result['Initial Catalog']:= 'master';
  Result['User Id']:= txtUsername.Text;
  Result['Password']:= txtPassword.Text;
end;

procedure TfrmConnection.ClearRecents;
begin
  cboServer.Items.Clear;
  FRecents.Clear;
end;

procedure TfrmConnection.RefreshRecents;
var
  R: TRegistry;
  L: TStringList;
  X: Integer;
  Rec: TRecentConn;
begin
  cboServer.Items.BeginUpdate;
  try
    ClearRecents;
    R:= TRegistry.Create(KEY_READ);
    try
      R.RootKey:= HKEY_CURRENT_USER;
      if R.KeyExists(REG_KEY_RECENT_CONN) then begin
        L:= TStringList.Create;
        try
          if R.OpenKey(REG_KEY_RECENT_CONN, False) then begin
            try
              R.GetKeyNames(L);
            finally
              R.CloseKey;
            end;
          end;
          for X := 0 to L.Count-1 do begin
            if R.OpenKey(REG_KEY_RECENT_CONN+L[X]+'\', False) then begin
              try
                Rec:= AddRecent(R.ReadString('ConnStr'));
                cboServer.Items.AddObject(Rec.Caption, Rec);
              finally
                R.CloseKey;
              end;
            end;
          end;
        finally
          L.Free;
        end;

      end else begin
        //Registry key does not exist

      end;
    finally
      R.Free;
    end;
  finally
    cboServer.Items.EndUpdate;
  end;
end;

procedure TfrmConnection.SetConnStr(const Value: TConnectionString);
begin
  cboServer.Text:= Value['Data Source'];
  txtUsername.Text:= Value['User Id'];
  txtPassword.Text:= Value['Password'];
end;

procedure TfrmConnection.txtUsernameChange(Sender: TObject);
begin
  BitBtn1.Enabled:= (cboServer.Text <> '') and (txtUsername.Text <> '');
end;

end.
