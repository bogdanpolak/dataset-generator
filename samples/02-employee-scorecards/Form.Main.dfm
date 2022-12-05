object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 462
  ClientWidth = 645
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
    Width = 639
    Height = 54
    Align = alTop
    Caption = 'Database Connect'
    TabOrder = 0
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
  end
  object gboxScorecards: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 63
    Width = 639
    Height = 234
    Align = alTop
    Caption = 'Employee Scorecards'
    TabOrder = 1
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
      ExplicitHeight = 195
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
        Height = 186
        Margins.Right = 0
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbxMonthsClick
        ExplicitWidth = 118
        ExplicitHeight = 170
      end
    end
    object scrboxScorecards: TScrollBox
      AlignWithMargins = True
      Left = 134
      Top = 18
      Width = 355
      Height = 135
      Margins.Left = 0
      Margins.Bottom = 6
      VertScrollBar.Tracking = True
      BevelInner = bvNone
      BevelKind = bkFlat
      BorderStyle = bsNone
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 421
    Width = 645
    Height = 41
    Align = alBottom
    Caption = 'lb'
    TabOrder = 2
    ExplicitLeft = 240
    ExplicitTop = 232
    ExplicitWidth = 185
    object ProgressBar1: TProgressBar
      Left = 11
      Top = 16
      Width = 265
      Height = 9
      Style = pbstMarquee
      TabOrder = 0
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
end
