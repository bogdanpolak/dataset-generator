unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Vcl.ExtCtrls, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, Vcl.Grids, Vcl.DBGrids, FireDAC.UI.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.VCLUI.Wait, FireDAC.DApt, Vcl.StdCtrls, Vcl.DBCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    FDMemTable1: TFDMemTable;
    DataSource1: TDataSource;
    Timer1: TTimer;
    DBGrid1: TDBGrid;
    FDMemTable1ID: TIntegerField;
    FDMemTable1Text: TWideStringField;
    FDMemTable1Date1: TDateField;
    FDMemTable1Float1: TFloatField;
    FDMemTable1Currency1: TCurrencyField;
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    FDQuery1OrderID: TFDAutoIncField;
    FDQuery1CustomerID: TStringField;
    FDQuery1CompanyName: TStringField;
    FDQuery1EmployeeID: TIntegerField;
    FDQuery1EmployeeName: TWideStringField;
    FDQuery1OrderDate: TDateTimeField;
    FDQuery1RequiredDate: TDateTimeField;
    FDQuery1ShippedDate: TDateTimeField;
    FDQuery1ShipVia: TIntegerField;
    FDQuery1Freight: TCurrencyField;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    procedure Timer1Timer(Sender: TObject);
  private
    procedure GenerateData1;
    procedure GenerateData2;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.GenerateData1;
begin
  FDMemTable1.CreateDataSet;
  FDMemTable1.InsertRecord([1, 'Ala ma kota', EncodeDate(2019, 09, 16),
    1.2, 1200]);
end;

procedure TForm1.GenerateData2;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(Self);
  with ds do
  begin
    // FieldDefs.Add('bDelete', ftBoolean);
    FieldDefs.Add('id', ftInteger);
    FieldDefs.Add('text1', ftWideString, 30);
    FieldDefs.Add('date1', ftDate);
    FieldDefs.Add('float1', ftFloat);
    FieldDefs.Add('currency1', ftCurrency);
    CreateDataSet;
    InsertRecord([1, 'Ala ma kota', EncodeDate(2019, 09, 16), 1.2, 1200]);
  end;
  DataSource1.DataSet := ds;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  GenerateData2;
end;

end.
