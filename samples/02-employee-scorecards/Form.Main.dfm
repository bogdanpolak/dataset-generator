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
    Height = 218
    Align = alTop
    Caption = 'Employee Scorecards'
    TabOrder = 1
    object Panel1: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 124
      Height = 195
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
        Width = 118
        Height = 170
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbxMonthsClick
      end
    end
    object MemoTest: TMemo
      AlignWithMargins = True
      Left = 135
      Top = 18
      Width = 499
      Height = 195
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Style = []
      Lines.Strings = (
        'MemoTest')
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 1
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
end
