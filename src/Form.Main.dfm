object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
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
  object PageControl1: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 628
    Height = 264
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 8
    ExplicitTop = 56
    ExplicitWidth = 618
    ExplicitHeight = 206
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      ExplicitLeft = 0
      ExplicitWidth = 281
      ExplicitHeight = 165
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
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      ExplicitWidth = 281
      ExplicitHeight = 165
      object Memo1: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 614
        Height = 230
        Align = alClient
        Lines.Strings = (
          'Memo1')
        TabOrder = 0
        ExplicitLeft = 48
        ExplicitTop = 40
        ExplicitWidth = 185
        ExplicitHeight = 89
      end
    end
  end
  object FDMemTable1: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 64
    Top = 120
    object FDMemTable1ID: TIntegerField
      FieldName = 'ID'
    end
    object FDMemTable1Text: TWideStringField
      FieldName = 'Text'
      Size = 50
    end
    object FDMemTable1Date1: TDateField
      FieldName = 'Date1'
    end
    object FDMemTable1Float1: TFloatField
      FieldName = 'Float1'
    end
    object FDMemTable1Currency1: TCurrencyField
      FieldName = 'Currency1'
    end
  end
  object DataSource1: TDataSource
    DataSet = FDMemTable1
    Left = 64
    Top = 168
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
