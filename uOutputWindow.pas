unit uOutputWindow;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  System.Generics.Collections,
  DB, ADODB, MidasLib, DBClient,
  SqlExec,
  uDatasetView,
  ChromeTabs,
  ChromeTabsTypes,
  ChromeTabsUtils,
  ChromeTabsControls,
  ChromeTabsThreadTimer,
  ChromeTabsClasses,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.ComCtrls,
  Vcl.DBGrids, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TDataResponse = class(TObject)
  private
    FBlock: TSqlExecBlock;
    function GetDataset(Index: Integer): TDataset;
  public
    constructor Creste(ABlock: TSqlExecBlock);
    destructor Destroy; override;
    function DatasetCount: Integer;
    property Datasets[Index: Integer]: TDataset read GetDataset;
  end;

  TfrmOutputWindow = class(TForm)
    MsgPages: TPageControl;
    tabMessages: TTabSheet;
    OutputBox: TRichEdit;
    tabData: TTabSheet;
    sbData: TScrollBox;
    Tabs: TChromeTabs;
    tabSearch: TTabSheet;
    actBlocks: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure TabsChange(Sender: TObject; ATab: TChromeTab;
      TabChangeType: TTabChangeType);
    procedure FormDestroy(Sender: TObject);
  private
    FDatasets: TObjectList<TfrmDatasetView>;
  public
    procedure PostMsg(const Text: String; const Style: TFontStyles = [];
      const Color: TColor = clBlack; const Detail: String = '');
  end;

var
  frmOutputWindow: TfrmOutputWindow;

implementation

{$R *.dfm}

{$IFDEF USE_V2}
uses
  uMain2;
{$ENDIF}

{ TDataResponse }

constructor TDataResponse.Creste(ABlock: TSqlExecBlock);
begin
  FBlock:= ABlock;
end;

destructor TDataResponse.Destroy;
begin
  inherited;
end;

function TDataResponse.DatasetCount: Integer;
begin
  Result:= 0; //TODO
end;

function TDataResponse.GetDataset(Index: Integer): TDataset;
begin
  Result:= nil; //TODO
end;

{ TfrmOutputWindow }

procedure TfrmOutputWindow.FormCreate(Sender: TObject);
var
  X: Integer;
begin
  OutputBox.Align:= alClient;
  MsgPages.Align:= alClient;
  for X := 0 to MsgPages.PageCount-1 do begin
    MsgPages.Pages[X].TabVisible:= False;
  end;
  MsgPages.ActivePageIndex:= 0;
  Tabs.ActiveTabIndex:= 0;

  FDatasets:= TObjectList<TfrmDatasetView>.Create(True);

end;

procedure TfrmOutputWindow.FormDestroy(Sender: TObject);
begin
  FDatasets.Clear; //TODO
  FreeAndNil(FDatasets);
end;

procedure TfrmOutputWindow.PostMsg(const Text: String; const Style: TFontStyles = [];
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

procedure TfrmOutputWindow.TabsChange(Sender: TObject; ATab: TChromeTab;
  TabChangeType: TTabChangeType);
begin
  //
end;

end.
