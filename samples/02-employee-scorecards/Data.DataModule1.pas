unit Data.DataModule1;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  System.DateUtils,
  Data.DB,
  Spring,
  Spring.Collections,
  {FireDAC}
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Stan.ExprFuncs, FireDAC.Stan.Param,
  FireDAC.Phys.Intf, FireDAC.Phys,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.UI.Intf, FireDAC.VCLUI.Wait, FireDAC.Comp.Client,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet;

type
  TDataModule1 = class(TDataModule)
    FDConnection1: TFDConnection;
    fdqOrdersInMonth: TFDQuery;
    fdqEmployees: TFDQuery;
    fdqDetailsInMonth: TFDQuery;
  private
  public
    constructor Create(aOwner: TComponent); override;
    procedure Connect();
    function GetActiveMonths: IList<String>;
    function IsConnected(): boolean;
    function GetDataSet_OrdersInMonth(aYear, aMonth: Word): TDataSet;
    function GetDataSet_DetailsInMonth(aYear, aMonth: Word): TDataSet;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

constructor TDataModule1.Create(aOwner: TComponent);
begin
  inherited;
  Assert(FDConnection1.Connected = False,
      'Error! Connection to database was active before opening');
end;

procedure TDataModule1.Connect();
begin
  FDConnection1.Open();
end;

function TDataModule1.GetActiveMonths: IList<String>;
var
  varMinDate: Variant;
  varMaxDate: Variant;
  aDate: TDateTime;
  aEndDate: TDateTime;
begin
  Result := TCollections.CreateList<String>();
  varMinDate := FDConnection1.ExecSQLScalar('SELECT Min(OrderDate) FROM {id Orders}');
  varMaxDate := FDConnection1.ExecSQLScalar('SELECT Max(OrderDate) FROM {id Orders}');
  if varMinDate = System.Variants.Null then
    Exit;
  if varMaxDate = System.Variants.Null then
    Exit;
  aDate := RecodeDay(VarToDateTime(varMinDate), 1);
  aEndDate := VarToDateTime(varMaxDate);
  while aDate <= aEndDate do
  begin
    Result.Add(FormatDateTime('yyyy-mm', aDate));
    aDate := IncMonth(aDate, 1);
  end;
end;

function TDataModule1.GetDataSet_OrdersInMonth(aYear: Word; aMonth: Word): TDataSet;
begin
  fdqOrdersInMonth.Close;
  fdqOrdersInMonth.ParamByName('YEAR').Value := aYear;
  fdqOrdersInMonth.ParamByName('MONTH').Value := aMonth;
  fdqOrdersInMonth.Open();
  Result := fdqOrdersInMonth;
end;

function TDataModule1.GetDataSet_DetailsInMonth(aYear: Word; aMonth: Word): TDataSet;
begin
  fdqDetailsInMonth.Close;
  fdqDetailsInMonth.ParamByName('YEAR').Value := aYear;
  fdqDetailsInMonth.ParamByName('MONTH').Value := aMonth;
  fdqDetailsInMonth.Open();
  Result := fdqDetailsInMonth;
end;

function TDataModule1.IsConnected: boolean;
begin
  Result := FDConnection1.Connected;
end;

end.
