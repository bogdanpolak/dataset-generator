object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Simple Generator'
  ClientHeight = 404
  ClientWidth = 735
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 233
    Top = 0
    Width = 5
    Height = 404
    ExplicitLeft = 266
    ExplicitTop = -8
  end
  object PageControl1: TPageControl
    AlignWithMargins = True
    Left = 238
    Top = 3
    Width = 494
    Height = 398
    Margins.Left = 0
    ActivePage = tshCode
    Align = alClient
    TabOrder = 1
    object tshCode: TTabSheet
      Caption = 'Code'
      ImageIndex = 1
      object Memo1: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 480
        Height = 364
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        Lines.Strings = (
          'Memo1')
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object pnSideBar: TPanel
    Left = 0
    Top = 0
    Width = 233
    Height = 404
    Align = alLeft
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 0
    object GroupBox1: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 230
      Height = 126
      Margins.Right = 0
      Align = alTop
      Caption = 'Generators'
      TabOrder = 0
      object bntGenSimpleDataset: TButton
        AlignWithMargins = True
        Left = 5
        Top = 18
        Width = 220
        Height = 25
        Align = alTop
        Caption = 'bntGenSimpleDataset (no options)'
        TabOrder = 0
        OnClick = bntGenSimpleDatasetClick
      end
      object btnGenerateOrders: TButton
        AlignWithMargins = True
        Left = 5
        Top = 49
        Width = 220
        Height = 25
        Align = alTop
        Caption = 'btnGenerateOrders'
        TabOrder = 1
        OnClick = btnGenerateOrdersClick
      end
      object btnGenerateLongLiterals: TButton
        AlignWithMargins = True
        Left = 5
        Top = 80
        Width = 220
        Height = 25
        Align = alTop
        Caption = 'btnGenerateLongLiterals'
        TabOrder = 2
        OnClick = btnGenerateLongLiteralsClick
      end
    end
    object GroupBox2: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 135
      Width = 227
      Height = 266
      Align = alClient
      Caption = 'Options'
      TabOrder = 1
    end
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=SQLite_Demo')
    LoginPrompt = False
    Left = 336
    Top = 64
  end
end
