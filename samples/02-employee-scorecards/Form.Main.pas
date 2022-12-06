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
  Vcl.ComCtrls, Vcl.ControlList, Vcl.WinXCtrls,
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
    Splitter1: TSplitter;
    tmrStart: TTimer;
    clistScorecards: TControlList;
    lblScorePosition: TLabel;
    lblScoreFullName: TLabel;
    lblScoreOrders: TLabel;
    lblScoreValues: TLabel;
    tmrLoadingScore: TTimer;
    procedure actDatabaseConnectExecute(Sender: TObject);
    procedure ActionList1Update(
      Action: TBasicAction;
      var Handled: Boolean);
    procedure clistScorecardsShowControl(
      const AIndex: Integer;
      AControl: TControl;
      var AVisible: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbxMonthsClick(Sender: TObject);
    procedure tmrLoadingScoreTimer(Sender: TObject);
    procedure tmrStartTimer(Sender: TObject);
  public const
    Version = '1.5';
  private
    fDataModule1: TDataModule1;
    fEmployees: IEnumerable<TEmployee>;
    fScoresDictionary: IDictionary<Integer, TEmployeeScore>;
    fLoadingIndex: Integer;
    fLoadingYear: Word;
    fLoadingMonth: Word;
    procedure FillListBoxWithMonths(const aListBox: TListBox);
    procedure StartLoadingScore(const aYear, aMonth: Word);
    procedure SetLoadingTimerInterval;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.actDatabaseConnectExecute(Sender: TObject);
begin
  fDataModule1.Connect();
  FillListBoxWithMonths(lbxMonths);
  fEmployees := fDataModule1.GetEmployees();
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
  activeMonths: IEnumerable<string>;
  aMonth: String;
begin
  activeMonths := fDataModule1.GetActiveMonths();
  aListBox.Clear;
  for aMonth in activeMonths do
    aListBox.AddItem(aMonth, nil);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fScoresDictionary := TCollections.CreateDictionary<Integer, TEmployeeScore>();
  gboxScorecards.Visible := False;
  gboxScorecards.Align := alClient;
  clistScorecards.Align := alClient;
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

function TryExtractMonthFromItem(
  const aItem: string;
  var aYear: Word;
  var aMonth: Word): Boolean;
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

procedure TForm1.clistScorecardsShowControl(
  const AIndex: Integer;
  AControl: TControl;
  var AVisible: Boolean);
var
  employee: TEmployee;
  score: TEmployeeScore;
  hasScore: Boolean;
begin
  if AIndex >= fEmployees.Count then
    Exit;
  employee := fEmployees.ElementAt(AIndex);
  lblScorePosition.Caption := employee.EmployeeId.ToString;
  lblScoreFullName.Caption := employee.FullName;
  hasScore := fScoresDictionary.TryGetValue(employee.EmployeeId, score);
  if hasScore then
  begin
    lblScoreOrders.Visible := True;
    lblScoreOrders.Caption := score.OrderCount.ToString;
    lblScoreValues.Caption := ValuesToStringPipes(score.OrderValues);
  end
  else
  begin
    lblScoreOrders.Visible := False;
    lblScoreValues.Caption := 'Calculating score ...';
  end;
end;

procedure TForm1.lbxMonthsClick(Sender: TObject);
var
  yy: Word;
  mm: Word;
begin
  if lbxMonths.ItemIndex < 0 then
    Exit;
  if TryExtractMonthFromItem(lbxMonths.Items[lbxMonths.ItemIndex], yy, mm) then
  begin
    StartLoadingScore(yy, mm);
  end;
end;

procedure TForm1.StartLoadingScore(const aYear, aMonth: Word);
begin
  fScoresDictionary.Clear;
  clistScorecards.Repaint;
  fLoadingIndex := 0;
  fLoadingYear := aYear;
  fLoadingMonth := aMonth;
  SetLoadingTimerInterval();
  tmrLoadingScore.Enabled := True;
end;

procedure TForm1.SetLoadingTimerInterval;
begin
  // Loading with delay
  // tmrLoadingScore.Interval := 200 + 50 * random(4);
  // Fast Loading
  tmrLoadingScore.Interval := 20;
end;

procedure TForm1.tmrLoadingScoreTimer(Sender: TObject);
var
  hasItem: Boolean;
  employee: TEmployee;
  score: TEmployeeScore;
  id: Integer;
begin
  tmrLoadingScore.Enabled := False;
  SetLoadingTimerInterval();
  hasItem := (fLoadingIndex >= 0) and (fLoadingIndex < fEmployees.Count);
  tmrLoadingScore.Enabled := hasItem;
  if not hasItem then
    Exit;
  employee := fEmployees.ElementAt(fLoadingIndex);
  id := employee.EmployeeId;
  score := fDataModule1.CalculateMonthlyScore(id, fLoadingYear, fLoadingMonth);
  fScoresDictionary.AddOrSetValue(id, score);
  clistScorecards.Repaint;
  inc(fLoadingIndex);
end;

procedure TForm1.tmrStartTimer(Sender: TObject);
begin
  tmrStart.Enabled := False;
  clistScorecards.ItemCount := fEmployees.Count;
end;

end.
