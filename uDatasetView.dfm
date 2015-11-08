object frmDatasetView: TfrmDatasetView
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsNone
  Caption = 'frmDatasetView'
  ClientHeight = 234
  ClientWidth = 468
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Grd: TDBGrid
    Left = 0
    Top = 78
    Width = 468
    Height = 130
    Align = alBottom
    DataSource = DS
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
    ReadOnly = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 468
    Height = 23
    Align = alTop
    BevelWidth = 2
    TabOrder = 1
    ExplicitLeft = 8
    ExplicitTop = 8
    DesignSize = (
      468
      23)
    object lblTitle: TLabel
      Left = 8
      Top = 4
      Width = 102
      Height = 13
      Caption = 'Script Result Data'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object SpeedButton1: TSpeedButton
      Left = 445
      Top = 0
      Width = 21
      Height = 20
      Cursor = crHandPoint
      Anchors = [akTop, akRight]
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C40E0000C40E00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC4C4C4A8A8A8C4C4C4FF00FFFF
        00FFFF00FFC4C4C4A8A8A8C4C4C4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        C4C4C4878793212155878793C4C4C4FF00FFC4C4C4878793212155878793C4C4
        C4FF00FFFF00FFFF00FFFF00FFC4C4C48888943131685D5DA9323268888894BD
        BDBD8888943232695D5DAA323268888894C4C4C4FF00FFFF00FFFF00FF8C8C97
        36366E6666B34F4FB26969B637376F6D6D8537376F6A6AB75151B46767B53636
        6E8C8C97FF00FFFF00FFFF00FF2F2F677878D95555B94F4FB55555B96E6EBC49
        49816F6FBC5656BA5252B85757BB8080E02F2F67FF00FFFF00FFFF00FFACACB8
        3838917C7CEF5B5BC05858BE5C5CC07878CB5D5DC15959BF5E5EC27F7FF03838
        91ACACB8FF00FFFF00FFFF00FFFF00FFADADBA42429A8686F16464C86060C661
        61C76161C76666CA8B8BF142429AADADBAFF00FFFF00FFFF00FFFF00FFFF00FF
        C4C4C47878914848A88383DB6666CC6868CF6868CF8686DC4848A8787891C4C4
        C4FF00FFFF00FFFF00FFFF00FFC4C4C490909C3C3C7F5D5DBD6D6DD47171D873
        73DB7373DB7171D86262C33D3D7F90909CC4C4C4FF00FFFF00FFFF00FF9393A0
        4242846060BF7070D77676DE8383E6C8C8F68585E87B7BE47676DE6666C64242
        849393A0FF00FFFF00FFFF00FF50508C9595E77676D97878E08787E9C0C0F877
        77D6C3C3F98C8CEF7F7FE88080E29C9CE950508CFF00FFFF00FFFF00FFB3B3C0
        6B6BBAAFAFF68D8DE9C3C3F96E6EBC9A9AB36E6EBCC9C9FA9494F1BABAF76C6C
        BAB3B3C0FF00FFFF00FFFF00FFFF00FFB4B4C17373BFB0B0FF7474C0B4B4C1FF
        00FFB4B4C17474C0B8B8FF7373BFB4B4C1FF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFB6B6C36C6CABB6B6C3FF00FFFF00FFFF00FFB6B6C36C6CABB6B6C3FF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      OnClick = SpeedButton1Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 227
    Width = 468
    Height = 7
    Cursor = crVSplit
    Align = alBottom
    BevelWidth = 2
    TabOrder = 2
    OnMouseDown = Panel2MouseDown
    OnMouseMove = Panel2MouseMove
    OnMouseUp = Panel2MouseUp
    ExplicitTop = 240
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 208
    Width = 468
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 100
      end
      item
        Width = 100
      end>
    ExplicitTop = 202
  end
  object DS: TDataSource
    DataSet = CDS
    Left = 104
    Top = 40
  end
  object CDS: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 56
    Top = 40
  end
end
