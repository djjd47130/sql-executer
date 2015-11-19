inherited frmContentHome: TfrmContentHome
  Caption = 'Home'
  ClientHeight = 428
  ClientWidth = 657
  OnCreate = FormCreate
  ExplicitWidth = 663
  ExplicitHeight = 457
  PixelsPerInch = 96
  TextHeight = 13
  object pLeft: TPanel [0]
    Left = 0
    Top = 0
    Width = 233
    Height = 428
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 0
      Top = 225
      Width = 233
      Height = 7
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 177
      ExplicitWidth = 39
    end
    object pRecentScripts: TPanel
      Left = 0
      Top = 232
      Width = 233
      Height = 196
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 217
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 233
        Height = 21
        Align = alTop
        Alignment = taLeftJustify
        Color = 15194573
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Padding.Left = 2
        ParentFont = False
        TabOrder = 0
        ExplicitWidth = 217
        object lblSelectedObject: TLabel
          Left = 3
          Top = 1
          Width = 119
          Height = 19
          Align = alLeft
          Caption = 'Recent Script Files'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          Layout = tlCenter
          ExplicitHeight = 16
        end
        object SpeedButton1: TSpeedButton
          Left = 211
          Top = 1
          Width = 21
          Height = 19
          Cursor = crHandPoint
          Align = alRight
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
          ExplicitLeft = 240
          ExplicitHeight = 20
        end
      end
      object lstRecents: TListView
        Left = 0
        Top = 21
        Width = 233
        Height = 92
        Align = alTop
        Columns = <
          item
            AutoSize = True
          end
          item
            Width = 0
          end>
        HotTrackStyles = [htHandPoint, htUnderlineHot]
        ReadOnly = True
        RowSelect = True
        ShowColumnHeaders = False
        TabOrder = 1
        ViewStyle = vsReport
        OnDblClick = lstRecentsDblClick
        ExplicitWidth = 217
      end
    end
    object pQuickActions: TPanel
      Left = 0
      Top = 0
      Width = 233
      Height = 153
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 217
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 233
        Height = 21
        Align = alTop
        Alignment = taLeftJustify
        Color = 15194573
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Padding.Left = 2
        ParentFont = False
        TabOrder = 0
        ExplicitWidth = 217
        object Label4: TLabel
          Left = 3
          Top = 1
          Width = 87
          Height = 19
          Align = alLeft
          Caption = 'Quick Actions'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          Layout = tlCenter
          ExplicitHeight = 16
        end
        object SpeedButton2: TSpeedButton
          Left = 211
          Top = 1
          Width = 21
          Height = 19
          Cursor = crHandPoint
          Align = alRight
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
          ExplicitLeft = 242
          ExplicitHeight = 20
        end
      end
      object BitBtn1: TBitBtn
        Left = 0
        Top = 21
        Width = 233
        Height = 25
        Action = frmSqlExec2.actFileNew
        Align = alTop
        Caption = 'New Script'
        Margin = 6
        Spacing = 6
        TabOrder = 1
        ExplicitWidth = 217
      end
      object BitBtn2: TBitBtn
        Left = 0
        Top = 46
        Width = 233
        Height = 25
        Action = frmSqlExec2.actFileOpen
        Align = alTop
        Caption = 'Open Script...'
        Margin = 6
        Spacing = 6
        TabOrder = 2
        ExplicitWidth = 217
      end
      object BitBtn3: TBitBtn
        Left = 0
        Top = 71
        Width = 233
        Height = 25
        Action = frmSqlExec2.actServerConnect
        Align = alTop
        Caption = 'Connect...'
        Margin = 6
        Spacing = 6
        TabOrder = 3
        ExplicitWidth = 217
      end
    end
  end
  inherited Acts: TActionList
    Images = dmDataModule.Imgs16
    Left = 320
    Top = 248
    object actNewScript: TAction
      Caption = 'Create New Script File'
      ImageIndex = 0
      OnExecute = actNewScriptExecute
    end
  end
end
