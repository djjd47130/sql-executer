unit uContentHome;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uContentBase, System.Actions,
  Vcl.ActnList, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TfrmContentHome = class(TfrmContentBase)
    actNewScript: TAction;
    pLeft: TPanel;
    Splitter2: TSplitter;
    pRecentScripts: TPanel;
    Panel4: TPanel;
    lblSelectedObject: TLabel;
    SpeedButton1: TSpeedButton;
    pQuickActions: TPanel;
    Panel1: TPanel;
    Label4: TLabel;
    SpeedButton2: TSpeedButton;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    lstRecents: TListView;
    procedure actNewScriptExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lstRecentsDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure RefreshRecents;
  end;

var
  frmContentHome: TfrmContentHome;

implementation

{$R *.dfm}

uses
  uDataModule
  , uMain2
  ;

procedure TfrmContentHome.actNewScriptExecute(Sender: TObject);
begin
  inherited;
  frmSqlExec2.actFileNew.Execute;
end;

procedure TfrmContentHome.FormCreate(Sender: TObject);
begin
  inherited;
  lstRecents.Align:= alClient;
end;

procedure TfrmContentHome.lstRecentsDblClick(Sender: TObject);
var
  I: TListItem;
  S: String;
begin
  inherited;
  //Open Recent
  I:= lstRecents.Selected;
  if Assigned(I) then begin
    S:= I.SubItems[0];
    frmSqlExec2.DoOpenFile(S);
  end;
end;

procedure TfrmContentHome.RefreshRecents;
var
  X: Integer;
  I: TListItem;
  S: String;
begin
  lstRecents.Items.BeginUpdate;
  try
    lstRecents.Items.Clear;
    for X := 0 to frmSqlExec2.MRU.Count-1 do begin
      S:= frmSqlExec2.MRU[X];
      I:= lstRecents.Items.Add;
      I.Caption:= ExtractFileName(S);
      I.SubItems.Add(S);
    end;
  finally
    lstRecents.Items.EndUpdate;
  end;
end;

end.
