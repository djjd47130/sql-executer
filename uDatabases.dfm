object frmDatabases: TfrmDatabases
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Select Databases'
  ClientHeight = 334
  ClientWidth = 294
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    294
    334)
  PixelsPerInch = 96
  TextHeight = 13
  object Lst: TCheckListBox
    Left = 12
    Top = 8
    Width = 263
    Height = 281
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 292
    Width = 294
    Height = 42
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 302
    ExplicitWidth = 304
    DesignSize = (
      294
      42)
    object BitBtn1: TBitBtn
      Left = 194
      Top = 8
      Width = 81
      Height = 25
      Anchors = [akTop, akRight]
      Kind = bkOK
      Margin = 6
      NumGlyphs = 2
      Spacing = 12
      TabOrder = 0
      ExplicitLeft = 204
    end
    object BitBtn2: TBitBtn
      Left = 107
      Top = 8
      Width = 81
      Height = 25
      Anchors = [akTop, akRight]
      Kind = bkCancel
      Margin = 6
      NumGlyphs = 2
      Spacing = 6
      TabOrder = 1
      ExplicitLeft = 117
    end
  end
end
