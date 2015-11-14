unit adpMRU;

{

TadpMRU

Full source code of a TadpMRU component, a non-visual
component which simplifies implementing a "Most Recently Used"
file list in a menu (or a popup menu). The TadpMRU component
allows for quick selection of a file that was recently accessed
(opened) in an application.

How to use:

http://delphi.about.com/library/weekly/aa112503a.htm


..............................................
Zarko Gajic, BSCS
About Guide to Delphi Programming
http://delphi.about.com
how to advertise: http://delphi.about.com/library/bladvertise.htm
free newsletter: http://delphi.about.com/library/blnewsletter.htm
forum: http://forums.about.com/ab-delphi/start/
..............................................
}

interface

uses
  Windows, Messages, SysUtils, Classes, Menus, Registry;

type

  TMRUClickEvent = procedure(Sender: TObject; const FileName: String) of object;

  TadpMRU = class(TComponent)
  private
    FItems : TStringList;

    FMaxItems: cardinal;
    FShowFullPath: boolean;
    FRegistryPath: string;
    FParentMenuItem: TMenuItem;
    FOnClick: TMRUClickEvent;
    procedure SetMaxItems(const Value: cardinal);
    procedure SetShowFullPath(const Value: boolean);
    procedure SetRegistryPath(const Value: string);
    procedure SetParentMenuItem(const Value: TMenuItem);

    procedure LoadMRU;
    procedure SaveMRU;
    procedure ItemsChange(Sender: TObject);
    procedure ClearParentMenu;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoClick(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddItem(const FileName: string);
    function RemoveItem(const FileName : string) : boolean;
  published
    property MaxItems: cardinal read FMaxItems write SetMaxItems default 4;
    property ShowFullPath: boolean read FShowFullPath write SetShowFullPath default True;
    property RegistryPath: string read FRegistryPath write SetRegistryPath;
    property ParentMenuItem: TMenuItem read FParentMenuItem write SetParentMenuItem;

    property OnClick: TMRUClickEvent read FOnClick write FOnClick;
  end;

procedure Register;

implementation

type
  TMRUMenuItem = class(TMenuItem); //to be able to recognize MRU menu item when deleting


procedure Register;
begin
  RegisterComponents('delphi.about.com', [TadpMRU]);
end;

{ TadpMRU }

constructor TadpMRU.Create(AOwner: TComponent);
begin
  inherited;
  FParentMenuItem := nil;
  FItems := TStringList.Create;
  FItems.OnChange := ItemsChange;

  FMaxItems := 4;
  FShowFullPath := True;
end; (*Create*)

procedure TadpMRU.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    if FRegistryPath <> '' then LoadMRU;
end; (*Loaded*)

destructor TadpMRU.Destroy;
begin
  if not (csDesigning in ComponentState) then SaveMRU;

  FItems.OnChange := nil;
  FItems.Free;

  inherited;
end; (*Destroy*)

procedure TadpMRU.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FParentMenuItem) then
    FParentMenuItem := nil;
end; (*Notification*)

procedure TadpMRU.AddItem(const FileName: string);
begin
  if FileName <> '' then
  begin
    FItems.BeginUpdate;
    try
      if FItems.IndexOf(FileName) > -1 then
        FItems.Delete(FItems.IndexOf(FileName));
      FItems.Insert(0, FileName);

      while FItems.Count > MaxItems do
        FItems.Delete(MaxItems);
    finally
      FItems.EndUpdate;
    end;
  end;
end; (*AddItem*)

function TadpMRU.RemoveItem(const FileName: string): boolean;
begin
  if FItems.IndexOf(FileName) > -1 then
  begin
    FItems.Delete(FItems.IndexOf(FileName));
    Result := True;
  end
  else
    Result := False;
end; (*RemoveItem*)


procedure TadpMRU.SetMaxItems(const Value: Cardinal);
begin
  if Value <> FMaxItems then
  begin
    if Value < 1 then FMaxItems := 1
    else
      if Value > MaxInt then
        FMaxItems := MaxInt - 1
      else
      begin
        FMaxItems := Value;
        FItems.BeginUpdate;
        try
          while FItems.Count > MaxItems do
            FItems.Delete(FItems.Count - 1);
        finally
          FItems.EndUpdate;
        end;
      end;
  end;
end; (*SetMaxItems*)

procedure TadpMRU.SetRegistryPath(const Value: string);
begin
  if FRegistryPath <> Value then
  begin
    FRegistryPath := Value;
    LoadMRU;
  end;
end; (*SetRegistryPath*)

procedure TadpMRU.SetShowFullPath(const Value: boolean);
begin
  if FShowFullPath <> Value then
  begin
    FShowFullPath := Value;
    ItemsChange(Self);
  end;
end; (*SetShowFullPath*)

procedure TadpMRU.LoadMRU;
var
  i: cardinal;
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(FRegistryPath, False) then
    begin
      FItems.BeginUpdate;
      FItems.Clear;
      try
        for i := 1 to FMaxItems do
          if ValueExists('MRU'+IntToStr(i)) then
            FItems.Add(ReadString('MRU'+IntToStr(i)));
      finally
        FItems.EndUpdate;
      end;
      CloseKey;
    end;
  finally
    Free;
  end;
end; (*LoadMRU*)

procedure TadpMRU.SaveMRU;
var
  i: integer;
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(FRegistryPath, True) then
    begin
      //delete old mru
      i:=1;
      while ValueExists('MRU'+IntToStr(i)) do
      begin
        DeleteValue('MRU'+IntToStr(i));
        Inc(i);
      end;

      //write new mru
      for i := 0 to -1 + FItems.Count do
        WriteString('MRU'+IntToStr(i+1),FItems[i]);
      CloseKey;
    end;
  finally
    Free;
  end;
end; (*SaveMRU*)


procedure TadpMRU.ItemsChange(Sender: TObject);
var
  i: Integer;
  NewMenuItem: TMenuItem;
  FileName: String;
begin
  if ParentMenuItem <> nil then
  begin
    ClearParentMenu;
    for i := 0 to -1 + FItems.Count do
    begin
      if ShowFullPath then
        FileName := StringReplace(FItems[I], '&', '&&', [rfReplaceAll, rfIgnoreCase])
      else
        FileName := StringReplace(ExtractFileName(FItems[i]), '&', '&&', [rfReplaceAll, rfIgnoreCase]);

      NewMenuItem := TMRUMenuItem.Create(Self);
      NewMenuItem.Caption := Format('%s', [FileName]);
      NewMenuItem.Tag := i;
      NewMenuItem.OnClick := DoClick;
      ParentMenuItem.Add(NewMenuItem);
    end;
  end;
end; (*ItemsChange*)

procedure TadpMRU.ClearParentMenu;
var
  i:integer;
begin
  if Assigned(ParentMenuItem) then
    for i:= -1 + ParentMenuItem.Count downto 0 do
      if ParentMenuItem.Items[i] is TMRUMenuItem then
        ParentMenuItem.Delete(i);
end; (*ClearParentMenu*)

procedure TadpMRU.DoClick(Sender: TObject);
begin
  if Assigned(FOnClick) and (Sender is TMRUMenuItem) then
    FOnClick(Self, FItems[TMRUMenuItem(Sender).Tag]);
end;(*DoClick*)

procedure TadpMRU.SetParentMenuItem(const Value: TMenuItem);
begin
  if FParentMenuItem <> Value then
  begin
    ClearParentMenu;
    FParentMenuItem := Value;
    ItemsChange(Self);
  end;
end; (*SetParentMenuItem*)

end.(*adpMRU.pas*)
