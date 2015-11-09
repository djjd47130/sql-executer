unit uDataModule;

interface

uses
  System.SysUtils, System.Classes, Vcl.ImgList, Vcl.Controls;

type
  TdmDataModule = class(TDataModule)
    Imgs16: TImageList;
    Imgs24: TImageList;
    Imgs32: TImageList;
    Imgs48: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmDataModule: TdmDataModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
