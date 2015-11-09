inherited frmContentScriptExec: TfrmContentScriptExec
  Caption = 'SQL Script'
  ClientHeight = 336
  ClientWidth = 649
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  ExplicitWidth = 655
  ExplicitHeight = 365
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter3: TSplitter [0]
    Left = 0
    Top = 229
    Width = 649
    Height = 7
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    Beveled = True
    ResizeStyle = rsUpdate
    ExplicitLeft = -1
    ExplicitTop = 310
    ExplicitWidth = 768
  end
  object pOutput: TPanel [1]
    Left = 0
    Top = 236
    Width = 649
    Height = 100
    Align = alBottom
    ParentBackground = False
    TabOrder = 0
    StyleElements = [seFont, seBorder]
    object pOutputTitle: TPanel
      Left = 1
      Top = 1
      Width = 647
      Height = 21
      Align = alTop
      Alignment = taLeftJustify
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = True
      ParentFont = False
      TabOrder = 0
      DesignSize = (
        647
        21)
      object lblOutputTitle: TLabel
        Left = 1
        Top = 1
        Width = 176
        Height = 19
        Align = alLeft
        AutoSize = False
        Caption = 'Script Output'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Layout = tlCenter
        ExplicitHeight = 23
      end
      object cmdOutputClose: TSpeedButton
        Left = 623
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
        ExplicitLeft = 457
      end
    end
  end
  object Stat: TStatusBar [2]
    Left = 0
    Top = 210
    Width = 649
    Height = 19
    Panels = <
      item
        Text = 'Disconnected'
        Width = 120
      end
      item
        Text = 'Ln 1  Col 1'
        Width = 120
      end
      item
        Text = 'Saved'
        Width = 120
      end
      item
        Text = '1 Block'
        Width = 120
      end
      item
        Width = 120
      end>
    OnDrawPanel = StatDrawPanel
  end
  object Panel2: TPanel [3]
    Left = 0
    Top = 0
    Width = 649
    Height = 44
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 3
      Width = 98
      Height = 13
      Caption = 'Current Connection:'
    end
    object Label2: TLabel
      Left = 159
      Top = 3
      Width = 90
      Height = 13
      Caption = 'Current Database:'
    end
    object Label5: TLabel
      Left = 388
      Top = 3
      Width = 82
      Height = 13
      Caption = 'Execute Method:'
    end
    object cboCurConn: TComboBox
      Left = 8
      Top = 20
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = '[Select Connection]'
      OnClick = cboCurConnClick
      Items.Strings = (
        '[Select Connection]')
    end
    object cboCurDatabase: TComboBox
      Left = 159
      Top = 20
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = '[Select Database]'
      OnClick = cboCurDatabaseClick
      Items.Strings = (
        '[Select Database]')
    end
    object BitBtn1: TBitBtn
      Left = 479
      Top = 20
      Width = 129
      Height = 21
      Cursor = crHandPoint
      Action = actExecSql
      Caption = 'Execute SQL'
      TabOrder = 2
    end
    object BitBtn2: TBitBtn
      Left = 307
      Top = 20
      Width = 78
      Height = 21
      Cursor = crHandPoint
      Action = actBatch
      Caption = 'Batch'
      TabOrder = 3
    end
    object cboCurExecMethod: TComboBox
      Left = 388
      Top = 20
      Width = 89
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 4
      Text = 'Execute'
      Items.Strings = (
        'Execute'
        'Datasets')
    end
  end
  object ED: TSynEdit [4]
    Left = 0
    Top = 44
    Width = 649
    Height = 85
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 3
    Gutter.AutoSize = True
    Gutter.DigitCount = 3
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = 7105644
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.RightOffset = 6
    Gutter.ShowLineNumbers = True
    Highlighter = SynSQL
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoTabsToSpaces, eoTrimTrailingSpaces]
    RightEdge = 120
    TabWidth = 4
    WantTabs = True
    OnChange = EDChange
    FontSmoothing = fsmNone
    ExplicitLeft = 1
    ExplicitTop = 47
  end
  object Prog: TProgressBar [5]
    Left = 491
    Top = 175
    Width = 94
    Height = 17
    TabOrder = 4
  end
  object SynSQL: TSynSQLSyn [6]
    Options.AutoDetectEnabled = True
    Options.AutoDetectLineLimit = 0
    Options.Visible = True
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clBlue
    PLSQLAttri.Foreground = clFuchsia
    StringAttri.Foreground = 198
    SQLDialect = sqlMSSQL2K
    Left = 80
    Top = 138
  end
  inherited Acts: TActionList
    Images = dmDataModule.Imgs16
    Left = 24
    Top = 136
    object actRefreshConnections: TAction [0]
      Caption = 'Refresh Connections'
      ImageIndex = 63
      OnExecute = actRefreshConnectionsExecute
    end
    object actRefreshDatabases: TAction [1]
      Caption = 'Refresh Databases'
      ImageIndex = 63
      OnExecute = actRefreshDatabasesExecute
    end
    object actBatch: TAction [2]
      Caption = 'Batch'
      ImageIndex = 40
      OnExecute = actBatchExecute
    end
    object actExecSql: TAction [3]
      Caption = 'Execute SQL'
      ImageIndex = 7
      OnExecute = actExecSqlExecute
    end
    object actSave: TAction
      Caption = 'Save'
      ImageIndex = 65
      OnExecute = actSaveExecute
    end
    object actSaveAs: TAction
      Caption = 'Save As'
      ImageIndex = 65
      OnExecute = actSaveAsExecute
    end
    object actUndo: TAction
      Caption = 'Undo'
      ImageIndex = 82
      OnExecute = actUndoExecute
    end
    object actFont: TAction
      Caption = 'Font'
      ImageIndex = 35
      OnExecute = actFontExecute
    end
  end
  object dlgSave: TSaveTextFileDialog
    Filter = 
      'SQL Script Files (*.sql)|*.sql|Text Files (*.txt)|*.txt|All File' +
      's (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 152
    Top = 140
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MinFontSize = 6
    MaxFontSize = 100
    Options = [fdEffects, fdNoFaceSel, fdNoStyleSel]
    Left = 216
    Top = 140
  end
  object tmrProg: TTimer
    Interval = 200
    OnTimer = tmrProgTimer
    Left = 280
    Top = 144
  end
end
