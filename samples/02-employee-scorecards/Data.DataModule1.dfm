object DataModule1: TDataModule1
  OldCreateOrder = False
  Height = 197
  Width = 215
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=SQLite_Demo')
    LoginPrompt = False
    Left = 40
    Top = 8
  end
  object fdqDetailsInMonth: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      
        'SELECT O.EmployeeId, {Year(O.OrderDate)} Year, {Month(O.OrderDat' +
        'e)} Month,'
      
        '  OD.OrderId, OD.ProductId, P.CategoryId, OD.UnitPrice, OD.Quant' +
        'ity, OD.Discount'
      'FROM Orders O'
      'INNER JOIN {id Order Details} OD ON O.OrderId = OD.OrderId'
      'INNER JOIN Products P ON P.ProductId = OD.ProductId'
      'WHERE EmployeeId = :EmployeeId Year = :Year and Month = :Month'
      'ORDER BY OrderDetails.OrderID')
    Left = 40
    Top = 80
    ParamData = <
      item
        Name = 'EMPLOYEEID'
        ParamType = ptInput
      end
      item
        Name = 'YEAR'
        DataType = ftString
        ParamType = ptInput
        Value = '1998'
      end
      item
        Name = 'MONTH'
        DataType = ftString
        ParamType = ptInput
        Value = '1'
      end>
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    Left = 144
    Top = 136
  end
end
