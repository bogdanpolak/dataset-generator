unit Form.Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.StrUtils,
  Spring.Collections,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Actions, Vcl.ActnList,
  {-}
  Data.DataModule1;

type
  TForm1 = class(TForm)
    gboxConnect: TGroupBox;
    btnConnect: TButton;
    gboxScorecards: TGroupBox;
    lbxMonths: TListBox;
    Panel1: TPanel;
    Label1: TLabel;
    ActionList1: TActionList;
    actDatabaseConnect: TAction;
    MemoTest: TMemo;
    procedure actDatabaseConnectExecute(Sender: TObject);
    procedure ActionList1Update(
      Action: TBasicAction;
      var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbxMonthsClick(Sender: TObject);
  public const
    Version = '1.5';
  private
    fDataModule1: TDataModule1;
    procedure FillListBoxWithMonths(const aListBox: TListBox);
    procedure ShowData(const aScores: IEnumerable<TEmployeeScore>);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Spring;

procedure TForm1.actDatabaseConnectExecute(Sender: TObject);
begin
  fDataModule1.Connect();
  FillListBoxWithMonths(lbxMonths);
end;

procedure TForm1.ActionList1Update(
  Action: TBasicAction;
  var Handled: Boolean);
begin
  if fDataModule1 = nil then
  begin
    gboxScorecards.Visible := False;
    actDatabaseConnect.Enabled := False;
  end
  else
  begin
    gboxScorecards.Visible := fDataModule1.IsConnected();
    actDatabaseConnect.Enabled := not fDataModule1.IsConnected();
  end;
end;

procedure TForm1.FillListBoxWithMonths(const aListBox: TListBox);
var
  activeMonths: IList<String>;
  aMonth: String;
begin
  activeMonths := fDataModule1.GetActiveMonths();
  aListBox.Clear;
  activeMonths.Reverse;
  for aMonth in activeMonths do
    aListBox.AddItem(aMonth, nil);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  gboxScorecards.Visible := False;
  gboxScorecards.Align := alClient;
  fDataModule1 := TDataModule1.Create(Application);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  actDatabaseConnect.Execute;
end;

function ValuesToStringPipes(const aValues: TArray<Currency>): string;
var
  v: Currency;
  idx: Integer;
  s: string;
begin
  s := '';
  for idx := 0 to High(aValues) do
  begin
    v := aValues[idx];
    s := s + '| ' + CurrToStrF(v, ffFixed, 2);
  end;
  Result := IfThen(aValues = nil, '', s + ' |');
end;

procedure TForm1.ShowData(const aScores: IEnumerable<TEmployeeScore>);
var
  score: TEmployeeScore;
  valuesText: string;
begin
  MemoTest.Clear;
  for score in aScores do
  begin
    valuesText := ValuesToStringPipes(score.OrderValues);
    MemoTest.Lines.Add(Format('%d:%s - orders:%d - %s', [score.EmployeeId,
      score.EmployeeName, score.OrderCount, valuesText]));
  end;
end;

function TryExtractMonthFromItem(
  const aItem: string;
  var aYear: word;
  var aMonth: word): Boolean;
var
  sYear, sMonth: string;
  iYear, iMonth: Integer;
begin
  if Length(aItem) <> 7 then
    Exit(False);
  sYear := aItem.Substring(0, 4);
  sMonth := aItem.Substring(5, 2);
  Result := TryStrToInt(sYear, iYear) and TryStrToInt(sMonth, iMonth);
  aYear := iYear;
  aMonth := iMonth;
end;

procedure TForm1.lbxMonthsClick(Sender: TObject);
var
  item: string;
  yy: word;
  mm: word;
  scores: IEnumerable<TEmployeeScore>;
begin
  if lbxMonths.ItemIndex < 0 then
    Exit;
  if TryExtractMonthFromItem(lbxMonths.Items[lbxMonths.ItemIndex], yy, mm) then
  begin
    scores := fDataModule1.CalculateMonthlyScorecards(yy, mm);
    ShowData(scores);
  end;
end;

end.
