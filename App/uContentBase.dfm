object frmContentBase: TfrmContentBase
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'SQL Exec Content Base Form'
  ClientHeight = 343
  ClientWidth = 638
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Acts: TActionList
    Left = 224
    Top = 112
    object actCloseTab: TAction
      Caption = 'Close Tab'
      ImageIndex = 48
      OnExecute = actCloseTabExecute
    end
  end
end
