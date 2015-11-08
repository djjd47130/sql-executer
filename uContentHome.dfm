inherited frmContentHome: TfrmContentHome
  Caption = 'Home'
  ClientHeight = 522
  ClientWidth = 657
  ExplicitWidth = 663
  ExplicitHeight = 551
  PixelsPerInch = 96
  TextHeight = 13
  object BitBtn1: TBitBtn [0]
    Left = 32
    Top = 56
    Width = 193
    Height = 25
    Action = actNewScript
    Caption = 'Create New Script File'
    Margin = 6
    Spacing = 6
    TabOrder = 0
  end
  inherited Acts: TActionList
    Images = frmSqlExec2.Imgs16
    Left = 264
    Top = 368
    object actNewScript: TAction
      Caption = 'Create New Script File'
      ImageIndex = 0
      OnExecute = actNewScriptExecute
    end
  end
end
