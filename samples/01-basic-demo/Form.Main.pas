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
    grbxOptions: TGroupBox;
    lblIndentation: TLabel;
    trbrIndentation: TTrackBar;
    rgrDatasetType: TRadioGroup;
    rgrAppendMode: TRadioGroup;
    GroupBox3: TGroupBox;
    edtMaxRows: TEdit;
    GroupBox4: TGroupBox;
    edtRightMargin: TEdit;
    procedure bntGenSimpleDatasetClick(Sender: TObject);
    procedure btnGenerateOrdersClick(Sender: TObject);
    procedure btnGenerateLongLiteralsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure trbrIndentationChange(Sender: TObject);
    procedure rgrDatasetTypeClick(Sender: TObject);
    procedure edtMaxRowsKeyPress(Sender: TObject; var Key: Char);
    procedure rgrAppendModeClick(Sender: TObject);
    procedure edtRightMarginKeyPress(Sender: TObject; var Key: Char);
  private const
    Version = '1.4';
  private
    fDSGenerator: TDSGenerator;
    LabelIndentation: string;
    constructor Create(AOwner: TComponent); override;
    function CreateSimpleMemTable: TDataSet;
    function CreateSqlQuery: TDataSet;
    procedure UpdateOptions;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

const
  DefaultMaxRows = 100;
  DefaultRightMargin = 60;

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

procedure TFormMain.UpdateOptions();
var
  maxRows: integer;
  rightMargin: Integer;
begin
  if grbxOptions.Enabled then
  begin
    fDSGenerator.IndentationText := StringOfChar(' ', trbrIndentation.Position);
    lblIndentation.Caption := Format(LabelIndentation,
      [trbrIndentation.Position]);
    case rgrDatasetType.ItemIndex of
      0: fDSGenerator.DataSetType := dstFDMemTable;
      else fDSGenerator.DataSetType := dstClientDataSet;
    end;
    case rgrAppendMode.ItemIndex of
      0: fDSGenerator.AppendMode := amMultilineAppends;
      1: fDSGenerator.AppendMode := amSinglelineAppends;
      else fDSGenerator.AppendMode := amAppendRows;
    end;
    if TryStrToInt(edtMaxRows.Text,maxRows) then
      fDSGenerator.MaxRows := maxRows
    else
    begin
      fDSGenerator.MaxRows := DefaultMaxRows;
      edtMaxRows.Text := DefaultMaxRows.ToString;
    end;
    if TryStrToInt(edtRightMargin.Text,rightMargin) then
      fDSGenerator.RightMargin := rightMargin
    else
    begin
      fDSGenerator.RightMargin := DefaultRightMargin;
      edtRightMargin.Text := DefaultRightMargin.ToString;
    end;
    fDSGenerator.Execute;
    Memo1.Lines := fDSGenerator.Code;
  end;
end;

procedure TFormMain.edtMaxRowsKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    UpdateOptions;
    Key := #0;
  end;
end;

procedure TFormMain.edtRightMarginKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    UpdateOptions;
    Key := #0;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  LabelIndentation := lblIndentation.Caption;
  grbxOptions.Enabled := False;
  try
    trbrIndentation.Position := 2;
    rgrDatasetType.ItemIndex := 0;
    rgrAppendMode.ItemIndex := 0;
    edtMaxRows.Text := DefaultMaxRows.ToString;
    edtRightMargin.Text := DefaultRightMargin.ToString;
  finally
    grbxOptions.Enabled := True;
    UpdateOptions;
  end;
end;

procedure TFormMain.rgrAppendModeClick(Sender: TObject);
begin
    UpdateOptions;
end;

procedure TFormMain.rgrDatasetTypeClick(Sender: TObject);
begin
    UpdateOptions;
end;

procedure TFormMain.trbrIndentationChange(Sender: TObject);
begin
    UpdateOptions;
end;

procedure TFormMain.bntGenSimpleDatasetClick(Sender: TObject);
begin
  fDSGenerator.DataSet := CreateSimpleMemTable;
  fDSGenerator.Execute;
  Memo1.Lines := fDSGenerator.Code;
end;

procedure TFormMain.btnGenerateOrdersClick(Sender: TObject);
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
