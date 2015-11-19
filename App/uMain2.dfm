object frmSqlExec2: TfrmSqlExec2
  Left = 0
  Top = 0
  Caption = 'SQL Script Executer'
  ClientHeight = 494
  ClientWidth = 756
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MM
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Stat: TStatusBar
    Left = 0
    Top = 471
    Width = 756
    Height = 23
    Panels = <
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 120
      end
      item
        Width = 50
      end>
  end
  object pMain: TPanel
    Left = 0
    Top = 65
    Width = 756
    Height = 336
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 272
      Top = 0
      Width = 7
      Height = 336
      AutoSnap = False
      ResizeStyle = rsUpdate
      ExplicitLeft = 241
      ExplicitHeight = 203
    end
    object pLeft: TPanel
      Left = 0
      Top = 0
      Width = 272
      Height = 336
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object Splitter2: TSplitter
        Left = 0
        Top = 209
        Width = 272
        Height = 7
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 177
        ExplicitWidth = 39
      end
      object pSelected: TPanel
        Left = 0
        Top = 216
        Width = 272
        Height = 120
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 272
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
          object lblSelectedObject: TLabel
            Left = 3
            Top = 1
            Width = 56
            Height = 19
            Align = alLeft
            Caption = 'Selected'
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
            Left = 250
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
            OnClick = SpeedButton1Click
            ExplicitLeft = 240
            ExplicitHeight = 20
          end
        end
        object SelView: TStringGrid
          Left = 0
          Top = 21
          Width = 272
          Height = 99
          Align = alClient
          BevelOuter = bvNone
          BorderStyle = bsNone
          ColCount = 2
          DefaultColWidth = 80
          DefaultRowHeight = 18
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect, goThumbTracking]
          ScrollBars = ssVertical
          TabOrder = 1
          ColWidths = (
            115
            133)
        end
      end
      object pConnections: TPanel
        Left = 0
        Top = 0
        Width = 272
        Height = 153
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Panel1: TPanel
          Left = 0
          Top = 0
          Width = 272
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
          object Label4: TLabel
            Left = 3
            Top = 1
            Width = 79
            Height = 19
            Align = alLeft
            Caption = 'Connections'
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
            Left = 250
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
            OnClick = SpeedButton2Click
            ExplicitLeft = 242
            ExplicitHeight = 20
          end
        end
        object ToolBar1: TToolBar
          Left = 0
          Top = 21
          Width = 272
          Height = 26
          ButtonHeight = 25
          ButtonWidth = 25
          Caption = 'ToolBar1'
          Images = dmDataModule.Imgs16
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          object ToolButton16: TToolButton
            Left = 0
            Top = 0
            Cursor = crHandPoint
            Action = actServerConnect
          end
          object ToolButton5: TToolButton
            Left = 25
            Top = 0
            Cursor = crHandPoint
            Action = actServerDisconnect
          end
        end
        object TV: TTreeView
          Left = 0
          Top = 80
          Width = 272
          Height = 73
          Align = alBottom
          BevelOuter = bvNone
          BorderStyle = bsNone
          Indent = 20
          ReadOnly = True
          TabOrder = 2
          OnClick = TVClick
          OnDblClick = TVDblClick
          OnExpanding = TVExpanding
        end
      end
    end
    object pContent: TPanel
      Left = 408
      Top = 0
      Width = 348
      Height = 336
      Align = alRight
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 1
    end
  end
  object TB: TToolBar
    Left = 0
    Top = 32
    Width = 756
    Height = 33
    ButtonHeight = 32
    ButtonWidth = 32
    Caption = 'TB'
    Color = clBtnFace
    Images = dmDataModule.Imgs24
    ParentColor = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Transparent = False
    object cmdNewFile: TToolButton
      Left = 0
      Top = 0
      Cursor = crHandPoint
      Action = actFileNew
    end
    object cmdOpenFile: TToolButton
      Left = 32
      Top = 0
      Cursor = crHandPoint
      Action = actFileOpen
    end
    object cmdSaveFile: TToolButton
      Left = 64
      Top = 0
      Cursor = crHandPoint
      Action = actFileSave
    end
    object cmdUndo: TToolButton
      Left = 96
      Top = 0
      Cursor = crHandPoint
      Action = actEditUndo
    end
    object ToolButton4: TToolButton
      Left = 128
      Top = 0
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 39
      Style = tbsSeparator
    end
    object cmdFont: TToolButton
      Left = 136
      Top = 0
      Cursor = crHandPoint
      Action = actScriptFont
    end
    object ToolButton7: TToolButton
      Left = 168
      Top = 0
      Width = 8
      Caption = 'ToolButton7'
      ImageIndex = 36
      Style = tbsSeparator
    end
    object cmdFind: TToolButton
      Left = 176
      Top = 0
      Cursor = crHandPoint
      Hint = 'Find Text'
      Caption = 'Find...'
      ImageIndex = 67
      Visible = False
    end
    object cmdFindPrev: TToolButton
      Left = 208
      Top = 0
      Cursor = crHandPoint
      Caption = 'Find Previous'
      ImageIndex = 93
      Visible = False
    end
    object cmdFindNext: TToolButton
      Left = 240
      Top = 0
      Cursor = crHandPoint
      Hint = 'Find Next in Text'
      Caption = 'Find Next'
      ImageIndex = 92
      Visible = False
    end
    object cmdFindReplace: TToolButton
      Left = 272
      Top = 0
      Cursor = crHandPoint
      Hint = 'Replace Text'
      Caption = 'Replace...'
      ImageIndex = 97
      Visible = False
    end
    object ToolButton12: TToolButton
      Left = 304
      Top = 0
      Width = 8
      Caption = 'ToolButton12'
      ImageIndex = 98
      Style = tbsSeparator
      Visible = False
    end
    object ToolButton14: TToolButton
      Left = 312
      Top = 0
      Cursor = crHandPoint
      Action = actServerConnect
    end
    object ToolButton1: TToolButton
      Left = 344
      Top = 0
      Cursor = crHandPoint
      Action = actServerDisconnect
    end
    object ToolButton2: TToolButton
      Left = 376
      Top = 0
      Cursor = crHandPoint
      Caption = 'ToolButton2'
      ImageIndex = 90
      Visible = False
    end
    object ToolButton3: TToolButton
      Left = 408
      Top = 0
      Cursor = crHandPoint
      Caption = 'ToolButton3'
      ImageIndex = 90
      Visible = False
    end
  end
  object Tabs: TChromeTabs
    Left = 0
    Top = 0
    Width = 756
    Height = 32
    Cursor = crHandPoint
    OnActiveTabChanged = TabsActiveTabChanged
    OnButtonAddClick = TabsButtonAddClick
    OnButtonCloseTabClick = TabsButtonCloseTabClick
    OnTabPopupMenu = TabsTabPopupMenu
    ActiveTabIndex = -1
    Images = dmDataModule.Imgs16
    Options.Display.CloseButton.Offsets.Vertical = 6
    Options.Display.CloseButton.Offsets.Horizontal = 2
    Options.Display.CloseButton.Height = 16
    Options.Display.CloseButton.Width = 16
    Options.Display.CloseButton.AutoHide = True
    Options.Display.CloseButton.Visibility = bvAll
    Options.Display.CloseButton.AutoHideWidth = 20
    Options.Display.CloseButton.CrossRadialOffset = 4
    Options.Display.AddButton.Offsets.Vertical = 10
    Options.Display.AddButton.Offsets.Horizontal = 2
    Options.Display.AddButton.Height = 14
    Options.Display.AddButton.Width = 31
    Options.Display.AddButton.ShowPlusSign = True
    Options.Display.AddButton.Visibility = avRightFloating
    Options.Display.AddButton.HorizontalOffsetFloating = -3
    Options.Display.ScrollButtonLeft.Offsets.Vertical = 10
    Options.Display.ScrollButtonLeft.Offsets.Horizontal = 1
    Options.Display.ScrollButtonLeft.Height = 15
    Options.Display.ScrollButtonLeft.Width = 15
    Options.Display.ScrollButtonRight.Offsets.Vertical = 10
    Options.Display.ScrollButtonRight.Offsets.Horizontal = 1
    Options.Display.ScrollButtonRight.Height = 15
    Options.Display.ScrollButtonRight.Width = 15
    Options.Display.TabModifiedGlow.Style = msRightToLeft
    Options.Display.TabModifiedGlow.VerticalOffset = -6
    Options.Display.TabModifiedGlow.Height = 30
    Options.Display.TabModifiedGlow.Width = 100
    Options.Display.TabModifiedGlow.AnimationPeriodMS = 4000
    Options.Display.TabModifiedGlow.EaseType = ttEaseInOutQuad
    Options.Display.TabModifiedGlow.AnimationUpdateMS = 50
    Options.Display.Tabs.SeeThroughTabs = False
    Options.Display.Tabs.TabOverlap = 15
    Options.Display.Tabs.ContentOffsetLeft = 18
    Options.Display.Tabs.ContentOffsetRight = 16
    Options.Display.Tabs.OffsetLeft = 0
    Options.Display.Tabs.OffsetTop = 4
    Options.Display.Tabs.OffsetRight = 0
    Options.Display.Tabs.OffsetBottom = 0
    Options.Display.Tabs.MinWidth = 120
    Options.Display.Tabs.MaxWidth = 220
    Options.Display.Tabs.TabWidthFromContent = False
    Options.Display.Tabs.PinnedWidth = 42
    Options.Display.Tabs.ImageOffsetLeft = 13
    Options.Display.Tabs.TextTrimType = tttFade
    Options.Display.Tabs.Orientation = toTop
    Options.Display.Tabs.BaseLineTabRegionOnly = False
    Options.Display.Tabs.WordWrap = False
    Options.Display.Tabs.TextAlignmentHorizontal = taLeftJustify
    Options.Display.Tabs.TextAlignmentVertical = taVerticalCenter
    Options.Display.Tabs.ShowImages = True
    Options.Display.Tabs.ShowPinnedTabText = False
    Options.Display.TabContainer.TransparentBackground = True
    Options.Display.TabContainer.OverlayButtons = True
    Options.Display.TabContainer.PaddingLeft = 0
    Options.Display.TabContainer.PaddingRight = 0
    Options.Display.TabMouseGlow.Offsets.Vertical = 0
    Options.Display.TabMouseGlow.Offsets.Horizontal = 0
    Options.Display.TabMouseGlow.Height = 200
    Options.Display.TabMouseGlow.Width = 200
    Options.Display.TabMouseGlow.Visible = True
    Options.Display.TabSpinners.Upload.ReverseDirection = True
    Options.Display.TabSpinners.Upload.RenderedAnimationStep = 2
    Options.Display.TabSpinners.Upload.Position.Offsets.Vertical = 0
    Options.Display.TabSpinners.Upload.Position.Offsets.Horizontal = 0
    Options.Display.TabSpinners.Upload.Position.Height = 16
    Options.Display.TabSpinners.Upload.Position.Width = 16
    Options.Display.TabSpinners.Upload.SweepAngle = 135
    Options.Display.TabSpinners.Download.ReverseDirection = False
    Options.Display.TabSpinners.Download.RenderedAnimationStep = 5
    Options.Display.TabSpinners.Download.Position.Offsets.Vertical = 0
    Options.Display.TabSpinners.Download.Position.Offsets.Horizontal = 0
    Options.Display.TabSpinners.Download.Position.Height = 16
    Options.Display.TabSpinners.Download.Position.Width = 16
    Options.Display.TabSpinners.Download.SweepAngle = 135
    Options.Display.TabSpinners.AnimationUpdateMS = 50
    Options.Display.TabSpinners.HideImagesWhenSpinnerVisible = True
    Options.DragDrop.DragType = dtWithinContainer
    Options.DragDrop.DragOutsideImageAlpha = 220
    Options.DragDrop.DragOutsideDistancePixels = 30
    Options.DragDrop.DragStartPixels = 2
    Options.DragDrop.DragControlImageResizeFactor = 0.500000000000000000
    Options.DragDrop.DragCursor = crDefault
    Options.DragDrop.DragDisplay = ddTabAndControl
    Options.DragDrop.DragFormBorderWidth = 2
    Options.DragDrop.DragFormBorderColor = 8421504
    Options.DragDrop.ContrainDraggedTabWithinContainer = True
    Options.Animation.DefaultMovementAnimationTimeMS = 150
    Options.Animation.DefaultStyleAnimationTimeMS = 400
    Options.Animation.AnimationTimerInterval = 15
    Options.Animation.MinimumTabAnimationWidth = 40
    Options.Animation.DefaultMovementEaseType = ttLinearTween
    Options.Animation.DefaultStyleEaseType = ttLinearTween
    Options.Animation.MovementAnimations.TabAdd.UseDefaultEaseType = True
    Options.Animation.MovementAnimations.TabAdd.UseDefaultAnimationTime = True
    Options.Animation.MovementAnimations.TabAdd.EaseType = ttEaseOutExpo
    Options.Animation.MovementAnimations.TabAdd.AnimationTimeMS = 500
    Options.Animation.MovementAnimations.TabDelete.UseDefaultEaseType = True
    Options.Animation.MovementAnimations.TabDelete.UseDefaultAnimationTime = True
    Options.Animation.MovementAnimations.TabDelete.EaseType = ttEaseOutExpo
    Options.Animation.MovementAnimations.TabDelete.AnimationTimeMS = 500
    Options.Animation.MovementAnimations.TabMove.UseDefaultEaseType = False
    Options.Animation.MovementAnimations.TabMove.UseDefaultAnimationTime = False
    Options.Animation.MovementAnimations.TabMove.EaseType = ttEaseOutExpo
    Options.Animation.MovementAnimations.TabMove.AnimationTimeMS = 500
    Options.Behaviour.BackgroundDblClickMaximiseRestoreForm = True
    Options.Behaviour.BackgroundDragMovesForm = True
    Options.Behaviour.TabSmartDeleteResizing = False
    Options.Behaviour.TabSmartDeleteResizeCancelDelay = 500
    Options.Behaviour.UseBuiltInPopupMenu = True
    Options.Behaviour.TabRightClickSelect = True
    Options.Behaviour.ActivateNewTab = True
    Options.Behaviour.DebugMode = False
    Options.Behaviour.IgnoreDoubleClicksWhileAnimatingMovement = True
    Options.Scrolling.Enabled = True
    Options.Scrolling.ScrollButtons = csbRight
    Options.Scrolling.ScrollStep = 20
    Options.Scrolling.ScrollRepeatDelay = 20
    Options.Scrolling.AutoHideButtons = True
    Options.Scrolling.DragScroll = True
    Options.Scrolling.DragScrollOffset = 50
    Options.Scrolling.MouseWheelScroll = True
    Tabs = <>
    LookAndFeel.TabsContainer.StartColor = 14586466
    LookAndFeel.TabsContainer.StopColor = 13201730
    LookAndFeel.TabsContainer.StartAlpha = 255
    LookAndFeel.TabsContainer.StopAlpha = 255
    LookAndFeel.TabsContainer.OutlineColor = 14520930
    LookAndFeel.TabsContainer.OutlineAlpha = 0
    LookAndFeel.Tabs.BaseLine.Color = 11110509
    LookAndFeel.Tabs.BaseLine.Thickness = 1.000000000000000000
    LookAndFeel.Tabs.BaseLine.Alpha = 255
    LookAndFeel.Tabs.Modified.CentreColor = clWhite
    LookAndFeel.Tabs.Modified.OutsideColor = clWhite
    LookAndFeel.Tabs.Modified.CentreAlpha = 130
    LookAndFeel.Tabs.Modified.OutsideAlpha = 0
    LookAndFeel.Tabs.DefaultFont.Name = 'Segoe UI'
    LookAndFeel.Tabs.DefaultFont.Color = clBlack
    LookAndFeel.Tabs.DefaultFont.Size = 9
    LookAndFeel.Tabs.DefaultFont.Alpha = 255
    LookAndFeel.Tabs.DefaultFont.TextRendoringMode = TextRenderingHintClearTypeGridFit
    LookAndFeel.Tabs.MouseGlow.CentreColor = clWhite
    LookAndFeel.Tabs.MouseGlow.OutsideColor = clWhite
    LookAndFeel.Tabs.MouseGlow.CentreAlpha = 120
    LookAndFeel.Tabs.MouseGlow.OutsideAlpha = 0
    LookAndFeel.Tabs.Spinners.Upload.Color = 12759975
    LookAndFeel.Tabs.Spinners.Upload.Thickness = 2.500000000000000000
    LookAndFeel.Tabs.Spinners.Upload.Alpha = 255
    LookAndFeel.Tabs.Spinners.Download.Color = 14388040
    LookAndFeel.Tabs.Spinners.Download.Thickness = 2.500000000000000000
    LookAndFeel.Tabs.Spinners.Download.Alpha = 255
    LookAndFeel.Tabs.Active.Font.Name = 'Segoe UI'
    LookAndFeel.Tabs.Active.Font.Color = clOlive
    LookAndFeel.Tabs.Active.Font.Size = 9
    LookAndFeel.Tabs.Active.Font.Alpha = 100
    LookAndFeel.Tabs.Active.Font.TextRendoringMode = TextRenderingHintClearTypeGridFit
    LookAndFeel.Tabs.Active.Font.UseDefaultFont = True
    LookAndFeel.Tabs.Active.Style.StartColor = 16645115
    LookAndFeel.Tabs.Active.Style.StopColor = clWhite
    LookAndFeel.Tabs.Active.Style.StartAlpha = 255
    LookAndFeel.Tabs.Active.Style.StopAlpha = 255
    LookAndFeel.Tabs.Active.Style.OutlineColor = 10189918
    LookAndFeel.Tabs.Active.Style.OutlineSize = 1.000000000000000000
    LookAndFeel.Tabs.Active.Style.OutlineAlpha = 255
    LookAndFeel.Tabs.NotActive.Font.Name = 'Segoe UI'
    LookAndFeel.Tabs.NotActive.Font.Color = 4603477
    LookAndFeel.Tabs.NotActive.Font.Size = 9
    LookAndFeel.Tabs.NotActive.Font.Alpha = 215
    LookAndFeel.Tabs.NotActive.Font.TextRendoringMode = TextRenderingHintClearTypeGridFit
    LookAndFeel.Tabs.NotActive.Font.UseDefaultFont = False
    LookAndFeel.Tabs.NotActive.Style.StartColor = 15194573
    LookAndFeel.Tabs.NotActive.Style.StopColor = 15194573
    LookAndFeel.Tabs.NotActive.Style.StartAlpha = 210
    LookAndFeel.Tabs.NotActive.Style.StopAlpha = 210
    LookAndFeel.Tabs.NotActive.Style.OutlineColor = 13546390
    LookAndFeel.Tabs.NotActive.Style.OutlineSize = 1.000000000000000000
    LookAndFeel.Tabs.NotActive.Style.OutlineAlpha = 215
    LookAndFeel.Tabs.Hot.Font.Name = 'Segoe UI'
    LookAndFeel.Tabs.Hot.Font.Color = 4210752
    LookAndFeel.Tabs.Hot.Font.Size = 9
    LookAndFeel.Tabs.Hot.Font.Alpha = 215
    LookAndFeel.Tabs.Hot.Font.TextRendoringMode = TextRenderingHintClearTypeGridFit
    LookAndFeel.Tabs.Hot.Font.UseDefaultFont = False
    LookAndFeel.Tabs.Hot.Style.StartColor = 15721176
    LookAndFeel.Tabs.Hot.Style.StopColor = 15589847
    LookAndFeel.Tabs.Hot.Style.StartAlpha = 255
    LookAndFeel.Tabs.Hot.Style.StopAlpha = 255
    LookAndFeel.Tabs.Hot.Style.OutlineColor = 12423799
    LookAndFeel.Tabs.Hot.Style.OutlineSize = 1.000000000000000000
    LookAndFeel.Tabs.Hot.Style.OutlineAlpha = 235
    LookAndFeel.CloseButton.Cross.Normal.Color = 6643031
    LookAndFeel.CloseButton.Cross.Normal.Thickness = 1.500000000000000000
    LookAndFeel.CloseButton.Cross.Normal.Alpha = 255
    LookAndFeel.CloseButton.Cross.Down.Color = 15461369
    LookAndFeel.CloseButton.Cross.Down.Thickness = 2.000000000000000000
    LookAndFeel.CloseButton.Cross.Down.Alpha = 220
    LookAndFeel.CloseButton.Cross.Hot.Color = clWhite
    LookAndFeel.CloseButton.Cross.Hot.Thickness = 2.000000000000000000
    LookAndFeel.CloseButton.Cross.Hot.Alpha = 220
    LookAndFeel.CloseButton.Circle.Normal.StartColor = clGradientActiveCaption
    LookAndFeel.CloseButton.Circle.Normal.StopColor = clNone
    LookAndFeel.CloseButton.Circle.Normal.StartAlpha = 0
    LookAndFeel.CloseButton.Circle.Normal.StopAlpha = 0
    LookAndFeel.CloseButton.Circle.Normal.OutlineColor = clGray
    LookAndFeel.CloseButton.Circle.Normal.OutlineSize = 1.000000000000000000
    LookAndFeel.CloseButton.Circle.Normal.OutlineAlpha = 0
    LookAndFeel.CloseButton.Circle.Down.StartColor = 3487169
    LookAndFeel.CloseButton.Circle.Down.StopColor = 3487169
    LookAndFeel.CloseButton.Circle.Down.StartAlpha = 255
    LookAndFeel.CloseButton.Circle.Down.StopAlpha = 255
    LookAndFeel.CloseButton.Circle.Down.OutlineColor = clGray
    LookAndFeel.CloseButton.Circle.Down.OutlineSize = 1.000000000000000000
    LookAndFeel.CloseButton.Circle.Down.OutlineAlpha = 255
    LookAndFeel.CloseButton.Circle.Hot.StartColor = 9408475
    LookAndFeel.CloseButton.Circle.Hot.StopColor = 9803748
    LookAndFeel.CloseButton.Circle.Hot.StartAlpha = 255
    LookAndFeel.CloseButton.Circle.Hot.StopAlpha = 255
    LookAndFeel.CloseButton.Circle.Hot.OutlineColor = 6054595
    LookAndFeel.CloseButton.Circle.Hot.OutlineSize = 1.000000000000000000
    LookAndFeel.CloseButton.Circle.Hot.OutlineAlpha = 255
    LookAndFeel.AddButton.Button.Normal.StartColor = 14340292
    LookAndFeel.AddButton.Button.Normal.StopColor = 14340035
    LookAndFeel.AddButton.Button.Normal.StartAlpha = 255
    LookAndFeel.AddButton.Button.Normal.StopAlpha = 255
    LookAndFeel.AddButton.Button.Normal.OutlineColor = 13088421
    LookAndFeel.AddButton.Button.Normal.OutlineSize = 1.000000000000000000
    LookAndFeel.AddButton.Button.Normal.OutlineAlpha = 255
    LookAndFeel.AddButton.Button.Down.StartColor = 13417645
    LookAndFeel.AddButton.Button.Down.StopColor = 13417644
    LookAndFeel.AddButton.Button.Down.StartAlpha = 255
    LookAndFeel.AddButton.Button.Down.StopAlpha = 255
    LookAndFeel.AddButton.Button.Down.OutlineColor = 10852748
    LookAndFeel.AddButton.Button.Down.OutlineSize = 1.000000000000000000
    LookAndFeel.AddButton.Button.Down.OutlineAlpha = 255
    LookAndFeel.AddButton.Button.Hot.StartColor = 15524314
    LookAndFeel.AddButton.Button.Hot.StopColor = 15524314
    LookAndFeel.AddButton.Button.Hot.StartAlpha = 255
    LookAndFeel.AddButton.Button.Hot.StopAlpha = 255
    LookAndFeel.AddButton.Button.Hot.OutlineColor = 14927787
    LookAndFeel.AddButton.Button.Hot.OutlineSize = 1.000000000000000000
    LookAndFeel.AddButton.Button.Hot.OutlineAlpha = 255
    LookAndFeel.AddButton.PlusSign.Normal.StartColor = clWhite
    LookAndFeel.AddButton.PlusSign.Normal.StopColor = clWhite
    LookAndFeel.AddButton.PlusSign.Normal.StartAlpha = 255
    LookAndFeel.AddButton.PlusSign.Normal.StopAlpha = 255
    LookAndFeel.AddButton.PlusSign.Normal.OutlineColor = clGray
    LookAndFeel.AddButton.PlusSign.Normal.OutlineSize = 1.000000000000000000
    LookAndFeel.AddButton.PlusSign.Normal.OutlineAlpha = 255
    LookAndFeel.AddButton.PlusSign.Down.StartColor = clWhite
    LookAndFeel.AddButton.PlusSign.Down.StopColor = clWhite
    LookAndFeel.AddButton.PlusSign.Down.StartAlpha = 255
    LookAndFeel.AddButton.PlusSign.Down.StopAlpha = 255
    LookAndFeel.AddButton.PlusSign.Down.OutlineColor = clGray
    LookAndFeel.AddButton.PlusSign.Down.OutlineSize = 1.000000000000000000
    LookAndFeel.AddButton.PlusSign.Down.OutlineAlpha = 255
    LookAndFeel.AddButton.PlusSign.Hot.StartColor = clWhite
    LookAndFeel.AddButton.PlusSign.Hot.StopColor = clWhite
    LookAndFeel.AddButton.PlusSign.Hot.StartAlpha = 255
    LookAndFeel.AddButton.PlusSign.Hot.StopAlpha = 255
    LookAndFeel.AddButton.PlusSign.Hot.OutlineColor = clGray
    LookAndFeel.AddButton.PlusSign.Hot.OutlineSize = 1.000000000000000000
    LookAndFeel.AddButton.PlusSign.Hot.OutlineAlpha = 255
    LookAndFeel.ScrollButtons.Button.Normal.StartColor = 14735310
    LookAndFeel.ScrollButtons.Button.Normal.StopColor = 14274499
    LookAndFeel.ScrollButtons.Button.Normal.StartAlpha = 255
    LookAndFeel.ScrollButtons.Button.Normal.StopAlpha = 255
    LookAndFeel.ScrollButtons.Button.Normal.OutlineColor = 11507842
    LookAndFeel.ScrollButtons.Button.Normal.OutlineSize = 1.000000000000000000
    LookAndFeel.ScrollButtons.Button.Normal.OutlineAlpha = 255
    LookAndFeel.ScrollButtons.Button.Down.StartColor = 13417645
    LookAndFeel.ScrollButtons.Button.Down.StopColor = 13417644
    LookAndFeel.ScrollButtons.Button.Down.StartAlpha = 255
    LookAndFeel.ScrollButtons.Button.Down.StopAlpha = 255
    LookAndFeel.ScrollButtons.Button.Down.OutlineColor = 10852748
    LookAndFeel.ScrollButtons.Button.Down.OutlineSize = 1.000000000000000000
    LookAndFeel.ScrollButtons.Button.Down.OutlineAlpha = 255
    LookAndFeel.ScrollButtons.Button.Hot.StartColor = 15524314
    LookAndFeel.ScrollButtons.Button.Hot.StopColor = 15524313
    LookAndFeel.ScrollButtons.Button.Hot.StartAlpha = 255
    LookAndFeel.ScrollButtons.Button.Hot.StopAlpha = 255
    LookAndFeel.ScrollButtons.Button.Hot.OutlineColor = 14927788
    LookAndFeel.ScrollButtons.Button.Hot.OutlineSize = 1.000000000000000000
    LookAndFeel.ScrollButtons.Button.Hot.OutlineAlpha = 255
    LookAndFeel.ScrollButtons.Button.Disabled.StartColor = 14340036
    LookAndFeel.ScrollButtons.Button.Disabled.StopColor = 14274499
    LookAndFeel.ScrollButtons.Button.Disabled.StartAlpha = 150
    LookAndFeel.ScrollButtons.Button.Disabled.StopAlpha = 150
    LookAndFeel.ScrollButtons.Button.Disabled.OutlineColor = 11113341
    LookAndFeel.ScrollButtons.Button.Disabled.OutlineSize = 1.000000000000000000
    LookAndFeel.ScrollButtons.Button.Disabled.OutlineAlpha = 100
    LookAndFeel.ScrollButtons.Arrow.Normal.StartColor = clWhite
    LookAndFeel.ScrollButtons.Arrow.Normal.StopColor = clWhite
    LookAndFeel.ScrollButtons.Arrow.Normal.StartAlpha = 255
    LookAndFeel.ScrollButtons.Arrow.Normal.StopAlpha = 255
    LookAndFeel.ScrollButtons.Arrow.Normal.OutlineColor = clGray
    LookAndFeel.ScrollButtons.Arrow.Normal.OutlineSize = 1.000000000000000000
    LookAndFeel.ScrollButtons.Arrow.Normal.OutlineAlpha = 200
    LookAndFeel.ScrollButtons.Arrow.Down.StartColor = clWhite
    LookAndFeel.ScrollButtons.Arrow.Down.StopColor = clWhite
    LookAndFeel.ScrollButtons.Arrow.Down.StartAlpha = 255
    LookAndFeel.ScrollButtons.Arrow.Down.StopAlpha = 255
    LookAndFeel.ScrollButtons.Arrow.Down.OutlineColor = clGray
    LookAndFeel.ScrollButtons.Arrow.Down.OutlineSize = 1.000000000000000000
    LookAndFeel.ScrollButtons.Arrow.Down.OutlineAlpha = 200
    LookAndFeel.ScrollButtons.Arrow.Hot.StartColor = clWhite
    LookAndFeel.ScrollButtons.Arrow.Hot.StopColor = clWhite
    LookAndFeel.ScrollButtons.Arrow.Hot.StartAlpha = 255
    LookAndFeel.ScrollButtons.Arrow.Hot.StopAlpha = 255
    LookAndFeel.ScrollButtons.Arrow.Hot.OutlineColor = clGray
    LookAndFeel.ScrollButtons.Arrow.Hot.OutlineSize = 1.000000000000000000
    LookAndFeel.ScrollButtons.Arrow.Hot.OutlineAlpha = 200
    LookAndFeel.ScrollButtons.Arrow.Disabled.StartColor = clSilver
    LookAndFeel.ScrollButtons.Arrow.Disabled.StopColor = clSilver
    LookAndFeel.ScrollButtons.Arrow.Disabled.StartAlpha = 150
    LookAndFeel.ScrollButtons.Arrow.Disabled.StopAlpha = 150
    LookAndFeel.ScrollButtons.Arrow.Disabled.OutlineColor = clGray
    LookAndFeel.ScrollButtons.Arrow.Disabled.OutlineSize = 1.000000000000000000
    LookAndFeel.ScrollButtons.Arrow.Disabled.OutlineAlpha = 200
    Align = alTop
    ShowHint = True
    TabOrder = 3
  end
  object MM: TMainMenu
    Images = dmDataModule.Imgs16
    Left = 80
    Top = 400
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Action = actFileNew
      end
      object Open1: TMenuItem
        Action = actFileOpen
      end
      object Save1: TMenuItem
        Action = actFileSave
      end
      object Saveas1: TMenuItem
        Action = actFileSaveAs
      end
      object Close1: TMenuItem
        Action = actCloseScript
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mRecent: TMenuItem
        Caption = 'Recent'
      end
      object N1: TMenuItem
        Caption = '-'
        Visible = False
      end
      object Exit1: TMenuItem
        Action = actFileExit
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Undo1: TMenuItem
        Action = actEditUndo
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Cut1: TMenuItem
        Caption = 'Cut'
        Enabled = False
        Hint = 'Cut Text'
        ImageIndex = 20
      end
      object Copy1: TMenuItem
        Caption = 'Copy'
        Enabled = False
        Hint = 'Copy Text'
        ImageIndex = 19
      end
      object Paste1: TMenuItem
        Caption = 'Paste'
        Enabled = False
        Hint = 'Paste Text'
        ImageIndex = 16
      end
      object Delete1: TMenuItem
        Caption = 'Delete'
        Enabled = False
        Hint = 'Delete Text'
        ImageIndex = 21
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Find1: TMenuItem
        Caption = 'Find...'
        Hint = 'Find Text'
        ImageIndex = 67
        ShortCut = 16454
        Visible = False
      end
      object FindPrevious1: TMenuItem
        Caption = 'Find Previous'
        ImageIndex = 93
        ShortCut = 115
        Visible = False
      end
      object FindNext1: TMenuItem
        Caption = 'Find Next'
        Hint = 'Find Next in Text'
        ImageIndex = 92
        ShortCut = 114
        Visible = False
      end
      object Rreplace1: TMenuItem
        Caption = 'Replace...'
        Hint = 'Replace Text'
        ImageIndex = 97
        ShortCut = 16456
        Visible = False
      end
      object GoTo1: TMenuItem
        Caption = 'Go To...'
        Enabled = False
        Hint = 'Goto in Text'
        ImageIndex = 7
        ShortCut = 16455
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object SelectAll1: TMenuItem
        Caption = 'Select All'
        Hint = 'Select All Text'
        ShortCut = 16449
      end
    end
    object Script1: TMenuItem
      Caption = 'Script'
      object ExecuteScript1: TMenuItem
        Action = actScriptExec
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Font1: TMenuItem
        Action = actScriptFont
      end
    end
    object Server1: TMenuItem
      Caption = 'Server'
      object Connection1: TMenuItem
        Action = actServerConnect
      end
      object Disconnect1: TMenuItem
        Action = actServerDisconnect
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      OnClick = View1Click
      object ShowConnections1: TMenuItem
        Caption = 'Show Connections'
        Checked = True
        OnClick = ShowConnections1Click
      end
      object ShowSelectedObject1: TMenuItem
        Caption = 'Show Selected Object'
        Checked = True
        OnClick = ShowSelectedObject1Click
      end
      object ShowMessages1: TMenuItem
        Caption = 'Show Messages'
        Checked = True
      end
      object ShowLinesAffected1: TMenuItem
        Caption = 'Show Lines Affected'
        OnClick = ShowLinesAffected1Click
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
  object Acts: TActionManager
    ActionBars = <
      item
      end>
    LargeImages = dmDataModule.Imgs24
    Images = dmDataModule.Imgs16
    Left = 40
    Top = 400
    StyleName = 'Platform Default'
    object actFileNew: TAction
      Category = 'File'
      Caption = 'New Script'
      Hint = 'Create New Script File'
      ImageIndex = 24
      ShortCut = 16462
      OnExecute = actFileNewExecute
    end
    object actServerConnect: TAction
      Category = 'Server'
      Caption = 'Connect...'
      Hint = 'Connect to Server'
      ImageIndex = 0
      ShortCut = 16462
      OnExecute = actServerConnectExecute
    end
    object actServerDisconnect: TAction
      Category = 'Server'
      Caption = 'Disconnect'
      Enabled = False
      Hint = 'Disconnect from Server'
      ImageIndex = 21
      ShortCut = 16452
      OnExecute = actServerDisconnectExecute
    end
    object actHome: TAction
      Caption = 'Home'
      ImageIndex = 43
      OnExecute = actHomeExecute
    end
    object actFileOpen: TAction
      Category = 'File'
      Caption = 'Open Script...'
      ImageIndex = 33
      ShortCut = 16463
      OnExecute = actFileOpenExecute
    end
    object actFileSave: TAction
      Category = 'File'
      Caption = 'Save Script'
      ImageIndex = 65
      ShortCut = 16467
      OnExecute = actFileSaveExecute
    end
    object actFileSaveAs: TAction
      Category = 'File'
      Caption = 'Save As...'
      ImageIndex = 65
      OnExecute = actFileSaveAsExecute
    end
    object actCloseScript: TAction
      Category = 'File'
      Caption = 'Close Script'
      ImageIndex = 21
      OnExecute = actCloseScriptExecute
    end
    object actFileExit: TAction
      Category = 'File'
      Caption = 'Exit'
      Hint = 'Exit Application'
      ImageIndex = 48
      OnExecute = actFileExitExecute
    end
    object actEditUndo: TAction
      Category = 'Edit'
      Caption = 'Undo'
      Hint = 'Undo Last Change'
      ImageIndex = 82
      ShortCut = 16474
      OnExecute = actEditUndoExecute
    end
    object actScriptFont: TAction
      Category = 'Script'
      Caption = 'Script Font'
      ImageIndex = 35
      ShortCut = 16454
      OnExecute = actScriptFontExecute
    end
    object actScriptExec: TAction
      Category = 'Script'
      Caption = 'Execute Script'
      Hint = 'Execute Script on Selected Database(s)'
      ImageIndex = 44
      ShortCut = 116
      OnExecute = actScriptExecExecute
    end
  end
  object dlgOpen: TOpenTextFileDialog
    Filter = 
      'SQL Script Files (*.sql)|*.sql|Text Files (*.txt)|*.txt|All File' +
      's (*.*)|*.*'
    Left = 144
    Top = 400
  end
  object JumpList1: TJumpList
    AutoRefresh = True
    Enabled = True
    CustomCategories = <>
    ShowRecent = True
    TaskList = <>
    Left = 232
    Top = 408
  end
  object tmrFileChange: TTimer
    Enabled = False
    Left = 320
    Top = 408
  end
end
