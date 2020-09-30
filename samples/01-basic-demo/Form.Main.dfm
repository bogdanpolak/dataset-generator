object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Simple Generator'
  ClientHeight = 541
  ClientWidth = 735
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 233
    Top = 0
    Width = 5
    Height = 541
    ExplicitLeft = 266
    ExplicitTop = -8
    ExplicitHeight = 404
  end
  object PageControl1: TPageControl
    AlignWithMargins = True
    Left = 238
    Top = 3
    Width = 494
    Height = 535
    Margins.Left = 0
    ActivePage = tshCode
    Align = alClient
    TabOrder = 1
    ExplicitHeight = 398
    object tshCode: TTabSheet
      Caption = 'Code'
      ImageIndex = 1
      ExplicitHeight = 370
      object Memo1: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 480
        Height = 501
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        Lines.Strings = (
          'Memo1')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object pnSideBar: TPanel
    Left = 0
    Top = 0
    Width = 233
    Height = 541
    Align = alLeft
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 0
    ExplicitHeight = 404
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
    object grbxOptions: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 135
      Width = 227
      Height = 403
      Align = alClient
      Caption = 'Options'
      TabOrder = 1
      ExplicitLeft = 0
      ExplicitHeight = 266
      object lblIndentation: TLabel
        AlignWithMargins = True
        Left = 14
        Top = 18
        Width = 199
        Height = 13
        Margins.Left = 12
        Margins.Right = 12
        Align = alTop
        Caption = 'Indentation (spaces: %d)'
        ExplicitWidth = 124
      end
      object trbrIndentation: TTrackBar
        AlignWithMargins = True
        Left = 5
        Top = 37
        Width = 217
        Height = 45
        Align = alTop
        Max = 8
        Position = 2
        PositionToolTip = ptBottom
        TabOrder = 0
        OnChange = trbrIndentationChange
        ExplicitLeft = 56
        ExplicitTop = 43
        ExplicitWidth = 150
      end
      object rgrDatasetType: TRadioGroup
        AlignWithMargins = True
        Left = 12
        Top = 88
        Width = 203
        Height = 73
        Margins.Left = 10
        Margins.Right = 10
        Align = alTop
        Caption = 'DataSet type'
        Items.Strings = (
          'TFDMemTable'
          'TClientDataSet')
        TabOrder = 1
        OnClick = rgrDatasetTypeClick
        ExplicitLeft = 5
        ExplicitWidth = 217
      end
      object rgrAppendMode: TRadioGroup
        AlignWithMargins = True
        Left = 12
        Top = 167
        Width = 203
        Height = 98
        Margins.Left = 10
        Margins.Right = 10
        Align = alTop
        Caption = 'AppendMode'
        Items.Strings = (
          'MultilineAppends'
          'SinglelineAppends'
          'amAppendRows')
        TabOrder = 2
        OnClick = rgrAppendModeClick
      end
      object GroupBox3: TGroupBox
        AlignWithMargins = True
        Left = 12
        Top = 271
        Width = 203
        Height = 50
        Margins.Left = 10
        Margins.Right = 10
        Align = alTop
        Caption = 'MaxRows (type Enter to regenerate)'
        Padding.Top = 1
        Padding.Bottom = 4
        TabOrder = 3
        ExplicitLeft = 13
        object edtMaxRows: TEdit
          AlignWithMargins = True
          Left = 5
          Top = 19
          Width = 193
          Height = 22
          Align = alClient
          Alignment = taCenter
          TabOrder = 0
          Text = 'edtMaxRows'
          OnKeyPress = edtMaxRowsKeyPress
          ExplicitHeight = 21
        end
      end
      object GroupBox4: TGroupBox
        AlignWithMargins = True
        Left = 12
        Top = 327
        Width = 203
        Height = 50
        Margins.Left = 10
        Margins.Right = 10
        Align = alTop
        Caption = 'RightMargin (type Enter to regenerate)'
        Padding.Top = 1
        Padding.Bottom = 4
        TabOrder = 4
        ExplicitLeft = 3
        ExplicitTop = 324
        ExplicitWidth = 223
        object edtRightMargin: TEdit
          AlignWithMargins = True
          Left = 5
          Top = 19
          Width = 193
          Height = 22
          Align = alClient
          Alignment = taCenter
          TabOrder = 0
          Text = 'edtRightMargin'
          OnKeyPress = edtRightMarginKeyPress
          ExplicitHeight = 21
        end
      end
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
