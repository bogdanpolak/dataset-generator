object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 270
  ClientWidth = 634
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
  object PageControl1: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 628
    Height = 264
    ActivePage = tshData
    Align = alClient
    TabOrder = 0
    object tshData: TTabSheet
      Caption = 'Data'
      object DBGrid1: TDBGrid
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 614
        Height = 230
        Align = alClient
        DataSource = DataSource1
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
    object tshCode: TTabSheet
      Caption = 'Code'
      ImageIndex = 1
      object Memo1: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 614
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
      end
    end
  end
  object DataSource1: TDataSource
    Left = 72
    Top = 120
  end
  object Timer1: TTimer
    Interval = 1
    OnTimer = Timer1Timer
    Left = 200
    Top = 120
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=SQLite_Demo')
    Connected = True
    LoginPrompt = False
    Left = 336
    Top = 120
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'SELECT Orders.OrderID, '
      '  Orders.CustomerID, Customers.CompanyName,  Orders.EmployeeID, '
      '  Employees.FirstName||'#39' '#39'||Employees.LastName EmployeeName, '
      '  Orders.OrderDate, Orders.RequiredDate, Orders.ShippedDate, '
      '  Orders.ShipVia, Orders.Freight '
      'FROM {id Orders} Orders '
      '  INNER JOIN {id Employees} Employees '
      '    ON Orders.EmployeeID = Employees.EmployeeID '
      '  INNER JOIN {id Customers} Customers '
      '    ON Orders.CustomerID = Customers.CustomerID '
      'WHERE {year(OrderDate)} = 1997 '
      'ORDER BY Orders.OrderID ')
    Left = 336
    Top = 168
    object FDQuery1OrderID: TFDAutoIncField
      FieldName = 'OrderID'
      Origin = 'OrderID'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = True
    end
    object FDQuery1CustomerID: TStringField
      FieldName = 'CustomerID'
      Origin = 'CustomerID'
      FixedChar = True
      Size = 5
    end
    object FDQuery1CompanyName: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'CompanyName'
      Origin = 'CompanyName'
      ProviderFlags = []
      ReadOnly = True
      Size = 40
    end
    object FDQuery1EmployeeName: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'EmployeeName'
      Origin = 'EmployeeName'
      ProviderFlags = []
      ReadOnly = True
      Size = 32767
    end
    object FDQuery1EmployeeID: TIntegerField
      FieldName = 'EmployeeID'
      Origin = 'EmployeeID'
    end
    object FDQuery1OrderDate: TDateTimeField
      FieldName = 'OrderDate'
      Origin = 'OrderDate'
    end
    object FDQuery1RequiredDate: TDateTimeField
      FieldName = 'RequiredDate'
      Origin = 'RequiredDate'
    end
    object FDQuery1ShippedDate: TDateTimeField
      FieldName = 'ShippedDate'
      Origin = 'ShippedDate'
    end
    object FDQuery1ShipVia: TIntegerField
      FieldName = 'ShipVia'
      Origin = 'ShipVia'
    end
    object FDQuery1Freight: TCurrencyField
      FieldName = 'Freight'
      Origin = 'Freight'
    end
  end
end
