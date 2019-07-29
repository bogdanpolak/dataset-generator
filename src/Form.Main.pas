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

  Action.CreateMemTable;

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
    tshData: TTabSheet;
    tshCode: TTabSheet;
    Memo1: TMemo;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    actCreateMemTable: TCreateMemTableAction;
    procedure GenerateData1;
    procedure GenerateData2;
    procedure GenerateDataAndCodeFromDataSet (ds:TDataSet);
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
    InsertRecord([2, 'Ala ma kota', System.Variants.Null, Null, 950]);
  end;
  DataSource1.DataSet := ds;
end;

procedure TForm1.GenerateDataAndCodeFromDataSet(ds: TDataSet);
begin
  DataSource1.DataSet := actCreateMemTable.CreateFDMemTable(ds);
  actCreateMemTable.GenerateCode(ds);
  Memo1.Lines := actCreateMemTable.Code;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  actCreateMemTable := TCreateMemTableAction.Create(Self);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  GenerateDataAndCodeFromDataSet (FDQuery1);
end;

end.
