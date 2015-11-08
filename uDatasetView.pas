unit uDatasetView;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Types,
  Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.DBGrids, Vcl.StdCtrls,
  Vcl.ExtCtrls, Data.DB, Datasnap.DBClient, Vcl.Buttons, Vcl.ComCtrls;

type
  TfrmDatasetView = class(TForm)
    Grd: TDBGrid;
    Panel1: TPanel;
    lblTitle: TLabel;
    DS: TDataSource;
    CDS: TClientDataSet;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure Panel2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FStartHeight: Integer;
    FStartPos: TPoint;
    FResizing: Boolean;
    FSkip: Integer;
    function GetCaption: String;
    procedure SetCaption(const Value: String);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    property Caption: String read GetCaption write SetCaption;
  end;

var
  frmDatasetView: TfrmDatasetView;

implementation

{$R *.dfm}

procedure TfrmDatasetView.FormCreate(Sender: TObject);
begin
  Grd.Align:= alClient;
end;

procedure TfrmDatasetView.Panel2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton.mbLeft then begin
    FResizing:= True;
    FStartHeight:= Height;
    FStartPos:= Point(X, Y);
  end;
end;

procedure TfrmDatasetView.Panel2MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
const
  SKIP_AMT = 7;
begin
  if FResizing then begin
    Inc(FSkip);
    if FSkip > SKIP_AMT then begin
      Height:= FStartHeight + (FStartPos.Y + Y);
      FSkip:= 0;
      //Application.ProcessMessages;
    end;
  end;
end;

procedure TfrmDatasetView.Panel2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FResizing:= False;
  ReleaseCapture;
end;

function TfrmDatasetView.GetCaption: String;
begin
  Result:= inherited Caption;
end;

procedure TfrmDatasetView.SetCaption(const Value: String);
begin
  inherited Caption:= Value;
  lblTitle.Caption:= 'Dataset: ' + Value;
end;

procedure TfrmDatasetView.SpeedButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmDatasetView.CreateParams(var Params: TCreateParams);
begin
  //BorderStyle := bsNone;
  inherited;
  //Params.ExStyle := Params.ExStyle or WS_EX_STATICEDGE;
  //Params.Style := Params.Style or WS_SIZEBOX;
end;

end.
