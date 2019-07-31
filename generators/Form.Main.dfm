object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Simple Generator'
  ClientHeight = 270
  ClientWidth = 634
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
    Left = 177
    Top = 0
    Width = 5
    Height = 270
    ExplicitLeft = 188
  end
  object PageControl1: TPageControl
    AlignWithMargins = True
    Left = 182
    Top = 3
    Width = 449
    Height = 264
    Margins.Left = 0
    ActivePage = tshCode
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 3
    ExplicitWidth = 628
    object tshCode: TTabSheet
      Caption = 'Code'
      ImageIndex = 1
      ExplicitWidth = 620
      object Memo1: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 435
        Height = 230
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
        ExplicitWidth = 614
      end
    end
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 174
    Height = 264
    Margins.Right = 0
    Align = alLeft
    Caption = 'GroupBox1'
    TabOrder = 1
    object Button1: TButton
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 164
      Height = 25
      Align = alTop
      Caption = 'Button1'
      TabOrder = 0
      OnClick = Button1Click
      ExplicitLeft = 56
      ExplicitTop = 120
      ExplicitWidth = 75
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 5
      Top = 49
      Width = 164
      Height = 25
      Align = alTop
      Caption = 'Button2'
      TabOrder = 1
      OnClick = Button2Click
      ExplicitLeft = 99
      ExplicitTop = 144
      ExplicitWidth = 75
    end
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=SQLite_Demo')
    Connected = True
    LoginPrompt = False
    Left = 80
    Top = 200
  end
end
