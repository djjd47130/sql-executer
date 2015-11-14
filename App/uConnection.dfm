object frmConnection: TfrmConnection
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Database Connection'
  ClientHeight = 277
  ClientWidth = 296
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    296
    277)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 45
    Top = 21
    Width = 74
    Height = 13
    Caption = 'Server Address'
  end
  object Label3: TLabel
    Left = 45
    Top = 77
    Width = 48
    Height = 13
    Caption = 'Username'
  end
  object Label4: TLabel
    Left = 45
    Top = 133
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object cmdDeleteRecent: TSpeedButton
    Left = 249
    Top = 40
    Width = 22
    Height = 21
    Anchors = [akTop, akRight]
    Enabled = False
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
    OnClick = cmdDeleteRecentClick
    ExplicitLeft = 271
  end
  object cboServer: TComboBox
    Left = 45
    Top = 40
    Width = 200
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = cboServerChange
    OnClick = cboServerClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 235
    Width = 296
    Height = 42
    Align = alBottom
    TabOrder = 3
    ExplicitTop = 204
    ExplicitWidth = 300
    DesignSize = (
      296
      42)
    object BitBtn1: TBitBtn
      Left = 196
      Top = 8
      Width = 81
      Height = 25
      Anchors = [akTop, akRight]
      Enabled = False
      Kind = bkOK
      Margin = 6
      NumGlyphs = 2
      Spacing = 12
      TabOrder = 0
      ExplicitLeft = 200
    end
    object BitBtn2: TBitBtn
      Left = 109
      Top = 8
      Width = 81
      Height = 25
      Anchors = [akTop, akRight]
      Kind = bkCancel
      Margin = 6
      NumGlyphs = 2
      Spacing = 6
      TabOrder = 1
      ExplicitLeft = 113
    end
  end
  object txtUsername: TEdit
    Left = 45
    Top = 96
    Width = 200
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = txtUsernameChange
    ExplicitWidth = 204
  end
  object txtPassword: TEdit
    Left = 45
    Top = 152
    Width = 200
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    PasswordChar = '*'
    TabOrder = 2
    ExplicitWidth = 204
  end
  object chkSaveRecent: TCheckBox
    Left = 96
    Top = 192
    Width = 110
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Save to Recents'
    TabOrder = 4
  end
end
