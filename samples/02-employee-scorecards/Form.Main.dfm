object Form1: TForm1
  Left = 0
  Top = 0
  Margins.Bottom = 0
  Caption = 'Form1'
  ClientHeight = 480
  ClientWidth = 673
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object gboxConnect: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 667
    Height = 54
    Align = alTop
    Caption = 'Database Connect'
    TabOrder = 0
    ExplicitWidth = 639
    object btnConnect: TButton
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 164
      Height = 31
      Action = actDatabaseConnect
      Align = alLeft
      TabOrder = 0
    end
    object ToggleSwitch1: TToggleSwitch
      Left = 272
      Top = 21
      Width = 144
      Height = 23
      StateCaptions.CaptionOn = 'Loading: Fast'
      StateCaptions.CaptionOff = 'Loading: Standard'
      TabOrder = 1
    end
  end
  object gboxScorecards: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 63
    Width = 667
    Height = 234
    Align = alTop
    Caption = 'Employee Scorecards'
    TabOrder = 1
    ExplicitWidth = 639
    object Splitter1: TSplitter
      Left = 129
      Top = 15
      Width = 5
      Height = 217
      ExplicitHeight = 305
    end
    object Panel1: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 124
      Height = 211
      Margins.Right = 0
      Align = alLeft
      BevelOuter = bvNone
      Caption = 'Panel1'
      TabOrder = 0
      object Label1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 118
        Height = 13
        Align = alTop
        Alignment = taCenter
        Caption = 'Month (select)'
        ExplicitWidth = 69
      end
      object lbxMonths: TListBox
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 121
        Height = 189
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbxMonthsClick
        ExplicitHeight = 186
      end
    end
    object clistScorecards: TControlList
      AlignWithMargins = True
      Left = 148
      Top = 21
      Width = 477
      Height = 167
      ItemHeight = 38
      ItemMargins.Left = 0
      ItemMargins.Top = 0
      ItemMargins.Right = 0
      ItemMargins.Bottom = 0
      ParentColor = False
      TabOrder = 1
      OnShowControl = clistScorecardsShowControl
      object lblScorePosition: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 34
        Height = 32
        Margins.Right = 0
        Align = alLeft
        Alignment = taCenter
        AutoSize = False
        Caption = '19'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -24
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblScoreFullName: TLabel
        AlignWithMargins = True
        Left = 37
        Top = 11
        Width = 130
        Height = 24
        Margins.Left = 0
        Margins.Top = 11
        Margins.Right = 0
        Align = alLeft
        AutoSize = False
        Caption = 'lblScoreFullName'
        ExplicitLeft = 39
        ExplicitTop = 10
        ExplicitHeight = 25
      end
      object lblScoreOrders: TLabel
        AlignWithMargins = True
        Left = 167
        Top = 6
        Width = 24
        Height = 29
        Margins.Left = 0
        Margins.Top = 6
        Margins.Right = 0
        Align = alLeft
        AutoSize = False
        Caption = '7x'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitLeft = 165
      end
      object lblScoreValues: TLabel
        AlignWithMargins = True
        Left = 191
        Top = 11
        Width = 279
        Height = 24
        Margins.Left = 0
        Margins.Top = 11
        Align = alClient
        Caption = 'lblScoreValues'
        ExplicitTop = 6
        ExplicitWidth = 68
        ExplicitHeight = 13
      end
    end
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 544
    Top = 16
    object actDatabaseConnect: TAction
      Caption = 'Connect'
      OnExecute = actDatabaseConnectExecute
    end
  end
  object tmrStart: TTimer
    Interval = 1
    OnTimer = tmrStartTimer
    Left = 472
    Top = 16
  end
  object tmrLoadingScore: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmrLoadingScoreTimer
    Left = 184
    Top = 152
  end
end
