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
  object fdqOrdersInMonth: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'SELECT OrderID, EmployeeID, CustomerID, OrderDate,'
      '  {Year(OrderDate)} OrderYear, {Month(OrderDate)} OrderMonth '
      'FROM {id Orders}'
      'WHERE OrderYear = 1997 and OrderMonth = 1'
      'ORDER BY EmployeeID')
    Left = 128
    Top = 8
  end
  object fdqEmployees: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      
        'select EmployeeId, LastName, FirstName, Title, {CONCAT(FirstName' +
        ',CONCAT('#39' '#39',LastName))} FullName from Employees')
    Left = 128
    Top = 64
  end
  object fdqDetailsInMonth: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      
        'select OrderDetails.OrderID, ProductId, UnitPrice, Quantity, Dis' +
        'count, CustomerID,'
      '  {Year(OrderDate)} OrderYear, {Month(OrderDate)} OrderMonth'
      '  from {id Order Details} as OrderDetails'
      '  inner join Orders on Orders.OrderID = OrderDetails.OrderID'
      '  WHERE OrderYear=1997 and OrderMonth=1'
      '  ORDER BY CustomerID, OrderDetails.OrderID')
    Left = 128
    Top = 120
  end
end
