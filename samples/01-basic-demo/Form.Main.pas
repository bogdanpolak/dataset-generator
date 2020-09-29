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
  FireDAC.Phys.SQLiteWrapper.Stat,
  {}
  Comp.Generator.DataSetCode;

type
  TFormMain = class(TForm)
    FDConnection1: TFDConnection;
    PageControl1: TPageControl;
    tshCode: TTabSheet;
    Memo1: TMemo;
    GroupBox1: TGroupBox;
    Splitter1: TSplitter;
    bntGenSimpleDataset: TButton;
    btnGenerateOrders: TButton;
    btnGenerateLongLiterals: TButton;
    pnSideBar: TPanel;
    GroupBox2: TGroupBox;
    procedure bntGenSimpleDatasetClick(Sender: TObject);
    procedure btnGenerateOrdersClick(Sender: TObject);
    procedure btnGenerateLongLiteralsClick(Sender: TObject);
  private const
    Version = '1.4';
  private
    fDSGenerator: TDSGenerator;
    constructor Create(AOwner: TComponent); override;
    function CreateSimpleMemTable: TDataSet;
    function CreateSqlQuery: TDataSet;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;
  fDSGenerator := TDSGenerator.Create(nil);
end;

function TFormMain.CreateSimpleMemTable: TDataSet;
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

function TFormMain.CreateSqlQuery: TDataSet;
var
  ds: TFDQuery;
begin
  ds := TFDQuery.Create(Self);
  ds.Connection := FDConnection1;
  ds.Open('SELECT Orders.OrderID,  Orders.CustomerID,' +
    '   Customers.CompanyName, Orders.EmployeeID, ' +
    '   Employees.FirstName||'' ''||Employees.LastName EmployeeName,' +
    '   Orders.OrderDate, Orders.RequiredDate, Orders.ShippedDate,' +
    '   Orders.ShipVia, Orders.Freight' + ' FROM {id Orders} Orders ' +
    '   INNER JOIN {id Employees} Employees' +
    '     ON Orders.EmployeeID = Employees.EmployeeID ' +
    '   INNER JOIN {id Customers} Customers' +
    '     ON Orders.CustomerID = Customers.CustomerID ' +
    ' WHERE {year(OrderDate)} = 1997 ORDER BY Orders.OrderID ');
  Result := ds;
end;

procedure TFormMain.bntGenSimpleDatasetClick(Sender: TObject);
var
  ADataSet: TDataSet;
begin
  ADataSet := CreateSimpleMemTable;
  Memo1.Lines.Text := TDSGenerator.GenerateAsString (ADataSet);
end;

procedure TFormMain.btnGenerateOrdersClick(Sender: TObject);
var
  ADataSet: TDataSet;
begin
  fDSGenerator.DataSet := CreateSqlQuery;
  fDSGenerator.Execute;
  Memo1.Lines := fDSGenerator.Code;
end;

procedure TFormMain.btnGenerateLongLiteralsClick(Sender: TObject);
begin
  // TODO: Generate Long Literals DataSet (copy code from unit tests)
end;

end.
