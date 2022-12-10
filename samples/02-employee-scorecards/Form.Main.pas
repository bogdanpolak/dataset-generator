unit Form.Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.StrUtils,
  System.Math,
  System.Generics.Collections,
  System.Generics.Defaults,
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
    ToggleSwitch1: TToggleSwitch;
    lblTotal: TLabel;
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
    fScores: IList<TEmployeeScore>;
    fLoadingIndex: Integer;
    fLoadingYear: Word;
    fLoadingMonth: Word;
    fIsCalculating: Boolean;
    procedure FillListBoxWithMonths(const aListBox: TListBox);
    procedure OnStartLoadingScore(const aYear, aMonth: Word);
    procedure OnCompletedLoading;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Scorecards.Utils;

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
  fScores := TCollections.CreateObjectList<TEmployeeScore>();
  gboxScorecards.Visible := False;
  gboxScorecards.Align := alClient;
  clistScorecards.Align := alClient;
  fDataModule1 := TDataModule1.Create(Application);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  actDatabaseConnect.Execute;
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
begin
  if AIndex >= fEmployees.Count then
    Exit;
  employee := fEmployees.ElementAt(AIndex);
  lblScorePosition.Caption := employee.EmployeeId.ToString;
  lblScoreFullName.Caption := employee.FullName;
  score := fScores.FirstOrDefault(
    function(const aScore: TEmployeeScore): Boolean
    begin
      Result := aScore.EmployeeId = employee.EmployeeId
    end);
  if score <> nil then
  begin
    lblScoreOrders.Caption := score.OrderCount.ToString;
    lblTotal.Caption := IfThen(score.OrderCount = 0, '',
      FormatFloat('#,###.##', score.TotalOrdersSum));
    lblScoreValues.Caption := Iff(score.Stars < 0, '',
      DupeString('★', score.Stars)); // ★☆
  end
  else
  begin
    lblScoreOrders.Caption := '';
    lblTotal.Caption := IfThen(fIsCalculating, '...', '');
    lblScoreValues.Caption := IfThen(fIsCalculating, 'calculating ...', '');
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
    OnStartLoadingScore(yy, mm);
  end;
end;

procedure TForm1.OnStartLoadingScore(const aYear, aMonth: Word);
begin
  fScores.Clear;
  fIsCalculating := True;
  clistScorecards.Repaint;
  fLoadingIndex := 0;
  fLoadingYear := aYear;
  fLoadingMonth := aMonth;
  tmrLoadingScore.Interval := Iff(ToggleSwitch1.State = tssOn, 20,
    200 + 50 * random(4));
  tmrLoadingScore.Enabled := True;
end;

procedure TForm1.OnCompletedLoading();
var
  Stars: Integer;
begin
  fScores.Sort(TComparer<TEmployeeScore>.Construct(
    function(const L, R: TEmployeeScore): Integer
    begin
      Result := Iff(L.ScoreValue > R.ScoreValue, -1,
        Iff(L.ScoreValue = R.ScoreValue, 0, +1));
    end));
  Stars := 5;
  fScores.ForEach(
    procedure(const score: TEmployeeScore)
    begin
      score.Stars := Iff(score.OrderCount > 0, Stars, 0);
      if Stars > 0 then
        dec(Stars);
    end);
  clistScorecards.Repaint;
end;

procedure TForm1.tmrLoadingScoreTimer(Sender: TObject);
var
  hasItemToCalculate: Boolean;
  employee: TEmployee;
  score: TEmployeeScore;
  id: Integer;
begin
  tmrLoadingScore.Enabled := False;
  hasItemToCalculate := (fLoadingIndex >= 0) and
    (fLoadingIndex < fEmployees.Count);
  fIsCalculating := hasItemToCalculate;
  if not hasItemToCalculate then
  begin
    OnCompletedLoading();
    Exit;
  end;
  tmrLoadingScore.Interval := Iff(ToggleSwitch1.State = tssOn, 20,
    200 + 50 * random(4));
  tmrLoadingScore.Enabled := True;
  employee := fEmployees.ElementAt(fLoadingIndex);
  id := employee.EmployeeId;
  score := TEmployeeScore.Create;
  fScores.Add(score);
  fDataModule1.CalculateMonthlyScore(id, fLoadingYear, fLoadingMonth, score);
  clistScorecards.Repaint;
  inc(fLoadingIndex);
end;

procedure TForm1.tmrStartTimer(Sender: TObject);
begin
  tmrStart.Enabled := False;
  clistScorecards.ItemCount := fEmployees.Count;
end;

end.
