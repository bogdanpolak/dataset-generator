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
    fMemTableSimple: TFDMemTable;
    fMemTableOrders: TFDMemTable;
    fMemTableLiterals: TFDMemTable;
    constructor Create(AOwner: TComponent); override;
    function GivenDataSet_Simple: TDataSet;
    function GivenDataSet_Orders: TDataSet;
    function GivenDataSet_Literals: TDataSet;
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

function TFormMain.GivenDataSet_Simple: TDataSet;
begin
  if fMemTableSimple = nil then
  begin
    fMemTableSimple := TFDMemTable.Create(Self);
    with fMemTableSimple do
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
  end;
  fMemTableSimple.First;
  Result := fMemTableSimple;
end;

function TFormMain.GivenDataSet_Literals: TDataSet;
const
  Line1 = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam varius posuere risus, maximus tincidunt libero sollicitudin ut. Nunc ullamcorper lacinia semper. Suspendisse non egestas velit. '
    + 'Fusce lacus orci, vestibulum porta turpis at, mollis malesuada odio. Etiam venenatis gravida placerat. Phasellus sit amet tincidunt turpis. Suspendisse convallis sem non libero condimentum rhoncus. '
    + 'Sed fringilla aliquam tempor. Aenean vitae mauris eu nunc efficitur convallis. Sed lectus est, placerat ac est vel, porttitor convallis urna. Duis eget metus justo. '
    + 'Quisque quam felis, tincidunt quis dignissim nec, eleifend sed turpis. Phasellus luctus sodales elementum.';
  Line2 = 'Quisque aliquet purus nec ullamcorper varius. Nam risus nunc, dictum at odio in, cursus aliquam nulla. Nam quis vestibulum ante, ut tincidunt odio. '
    + 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque magna diam, porttitor vitae pharetra quis, convallis et nunc. Cras lacinia placerat velit vitae laoreet. '
    + 'Aenean vestibulum sodales metus, nec cursus odio pharetra nec. Morbi quis laoreet orci. Curabitur non odio vel diam tincidunt pulvinar. Ut sagittis arcu justo, at gravida diam finibus non. '
    + 'Nullam quam ante, ullamcorper sed tortor a, rutrum vehicula diam. In ac magna magna. Phasellus iaculis elit at posuere laoreet. In egestas blandit rhoncus. Nam at magna odio.';
begin
  if fMemTableLiterals = nil then
  begin
    fMemTableLiterals := TFDMemTable.Create(Self);
    with fMemTableLiterals do
    begin
      FieldDefs.Add('No', ftInteger, 0);
      FieldDefs.Add('Paragraph', ftWideString, 1000);
      CreateDataSet;
      AppendRecord([1, Line1]);
      AppendRecord([2, Line2]);
    end;
  end;
  fMemTableLiterals.First;
  Result := fMemTableLiterals;
end;

function TFormMain.GivenDataSet_Orders: TDataSet;
var
  fdqOrders: TFDQuery;
begin
  if fMemTableOrders = nil then
  begin
    fdqOrders := TFDQuery.Create(Self);
    try
      fdqOrders.Connection := FDConnection1;
      fdqOrders.Open('SELECT Orders.OrderID,  Orders.CustomerID,' +
        '   Customers.CompanyName, Orders.EmployeeID, ' +
        '   Employees.FirstName||'' ''||Employees.LastName EmployeeName,' +
        '   Orders.OrderDate, Orders.RequiredDate, Orders.ShippedDate,' +
        '   Orders.ShipVia, Orders.Freight' + ' FROM {id Orders} Orders ' +
        '   INNER JOIN {id Employees} Employees' +
        '     ON Orders.EmployeeID = Employees.EmployeeID ' +
        '   INNER JOIN {id Customers} Customers' +
        '     ON Orders.CustomerID = Customers.CustomerID ' +
        ' WHERE {year(OrderDate)} = 1997 ORDER BY Orders.OrderID ');
      fMemTableOrders := TFDMemTable.Create(Self);
      fMemTableOrders.CopyDataSet(fdqOrders, [coStructure, coRestart,
        coAppend]);
    finally
      fdqOrders.Free;
    end;
  end;
  fMemTableOrders.First;
  Result := fMemTableOrders;
end;

procedure TFormMain.UpdateOptions();
var
  maxRows: integer;
  rightMargin: integer;
begin
  if grbxOptions.Enabled then
  begin
    fDSGenerator.IndentationText := StringOfChar(' ', trbrIndentation.Position);
    lblIndentation.Caption := Format(LabelIndentation,
      [trbrIndentation.Position]);
    case rgrDatasetType.ItemIndex of
      0:
        fDSGenerator.DataSetType := dstFDMemTable;
    else
      fDSGenerator.DataSetType := dstClientDataSet;
    end;
    case rgrAppendMode.ItemIndex of
      0:
        fDSGenerator.AppendMode := amMultilineAppends;
      1:
        fDSGenerator.AppendMode := amSinglelineAppends;
    else
      fDSGenerator.AppendMode := amAppendRows;
    end;
    if TryStrToInt(edtMaxRows.Text, maxRows) then
      fDSGenerator.maxRows := maxRows
    else
    begin
      fDSGenerator.maxRows := DefaultMaxRows;
      edtMaxRows.Text := DefaultMaxRows.ToString;
    end;
    if TryStrToInt(edtRightMargin.Text, rightMargin) then
      fDSGenerator.rightMargin := rightMargin
    else
    begin
      fDSGenerator.rightMargin := DefaultRightMargin;
      edtRightMargin.Text := DefaultRightMargin.ToString;
    end;
    fDSGenerator.Execute;
    Memo1.Lines.Text := fDSGenerator.Code.Text;
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
  fMemTableSimple := nil;
  fMemTableOrders := nil;
  fMemTableLiterals := nil;
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
  fDSGenerator.DataSet := GivenDataSet_Simple;
  fDSGenerator.Execute;
  Memo1.Lines := fDSGenerator.Code;
end;

procedure TFormMain.btnGenerateOrdersClick(Sender: TObject);
begin
  fDSGenerator.DataSet := GivenDataSet_Orders;
  fDSGenerator.Execute;
  Memo1.Lines := fDSGenerator.Code;
end;

procedure TFormMain.btnGenerateLongLiteralsClick(Sender: TObject);
begin
  fDSGenerator.DataSet := GivenDataSet_Literals;
  fDSGenerator.Execute;
  Memo1.Lines := fDSGenerator.Code;
end;

end.
