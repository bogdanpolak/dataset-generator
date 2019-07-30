unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Grids,
  Vcl.DBGrids, Vcl.StdCtrls, Vcl.DBCtrls, Vcl.ComCtrls,
  Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait, FireDAC.DApt,
  Action.GenerateDataSetCode;

type
  TFormMain = class(TForm)
    Timer1: TTimer;
    FDConnection1: TFDConnection;
    PageControl1: TPageControl;
    tshCode: TTabSheet;
    Memo1: TMemo;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    actGenDataSetCode: TGenDataSetCodeAction;
    function CreateSimpleMemTable: TFDMemTable;
    function CreateSqlQuery: TFDQuery;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

function TFormMain.CreateSimpleMemTable: TFDMemTable;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(Self);
  with ds do
  begin
    FieldDefs.Add('id', ftInteger);
    FieldDefs.Add('text1', ftWideString, 30);
    FieldDefs.Add('date1', ftDate);
    FieldDefs.Add('float1', ftFloat);
    FieldDefs.Add('currency1', ftCurrency);
    CreateDataSet;
    AppendRecord([1, 'Ala ma kota', EncodeDate(2019, 09, 16), 1.2, 1200]);
    AppendRecord([2, 'Ala ma kota', System.Variants.Null, Null, 950]);
  end;
  ds.First;
  Result := ds;
end;

function TFormMain.CreateSqlQuery: TFDQuery;
begin
  Result := TFDQuery.Create(Self);
  Result.Connection := FDConnection1;
  Result.Open('SELECT Orders.OrderID,  Orders.CustomerID,' +
    '   Customers.CompanyName, Orders.EmployeeID, ' +
    '   Employees.FirstName||'' ''||Employees.LastName EmployeeName,' +
    '   Orders.OrderDate, Orders.RequiredDate, Orders.ShippedDate,' +
    '   Orders.ShipVia, Orders.Freight' + ' FROM {id Orders} Orders ' +
    '   INNER JOIN {id Employees} Employees' +
    '     ON Orders.EmployeeID = Employees.EmployeeID ' +
    '   INNER JOIN {id Customers} Customers' +
    '     ON Orders.CustomerID = Customers.CustomerID ' +
    ' WHERE {year(OrderDate)} = 1997 ORDER BY Orders.OrderID ');
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  actGenDataSetCode := TGenDataSetCodeAction.Create(Self);
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
var
  ds: TFDQuery;
begin
  Timer1.Enabled := False;
  // ----------------------------------
  ds := CreateSqlQuery;
  actGenDataSetCode.GenerateCode(ds);
  Memo1.Lines := actGenDataSetCode.Code;
end;

end.
