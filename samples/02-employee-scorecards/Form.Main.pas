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
  Vcl.ComCtrls,
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
    scrboxScorecards: TScrollBox;
    ProgressBar1: TProgressBar;
    Panel2: TPanel;
    tmrStart: TTimer;
    procedure actDatabaseConnectExecute(Sender: TObject);
    procedure ActionList1Update(
      Action: TBasicAction;
      var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbxMonthsClick(Sender: TObject);
    procedure tmrStartTimer(Sender: TObject);
  public const
    Version = '1.5';
  private
    fDataModule1: TDataModule1;
    fEmployees: IEnumerable<TEmployee>;
    procedure FillListBoxWithMonths(const aListBox: TListBox);
    procedure BuildScorecardPanels(const aEmployees: IEnumerable<TEmployee>);
    procedure UpdateScorecardPanel(const score: TEmployeeScore);
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
  gboxScorecards.Visible := False;
  gboxScorecards.Align := alClient;
  scrboxScorecards.Align := alClient;
  scrboxScorecards.BorderStyle := bsNone;
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

procedure TForm1.BuildScorecardPanels(const aEmployees: IEnumerable<TEmployee>);
var
  parentControl: TWinControl;
  employee: TEmployee;
  rowPanel: TPanel;
  empIdLablel: TLabel;
  empFullNameLabel: TLabel;
  empValuesLabel: TLabel;
begin
  parentControl := scrboxScorecards;
  while parentControl.ControlCount > 0 do
  begin
    parentControl.Controls[0].Free;
  end;
  for employee in aEmployees do
  begin
    rowPanel := TPanel.Create(parentControl);
    with rowPanel do
    begin
      Name := Format('PanelScorecards%d', [employee.EmployeeId]);
      Parent := parentControl;
      Height := 35;
      Align := alTop;
      BorderStyle := bsSingle;
      BorderWidth := 1;
      Caption := '';
    end;
    empIdLablel := TLabel.Create(rowPanel);
    with empIdLablel do
    begin
      Name := Format('Score%d_Position', [employee.EmployeeId]);
      Align := alLeft;
      Width := 28;
      AutoSize := false;
      Font.Size := 17;
      AlignWithMargins := True;
      Margins.Left := 8;
      Margins.Right := 0;
      Margins.Top := 0;
      Margins.Bottom := 0;
      Parent := rowPanel;
      Caption := employee.EmployeeId.ToString;
    end;
    empFullNameLabel := TLabel.Create(rowPanel);
    with empFullNameLabel do
    begin
      Name := Format('Score%d_FullName', [employee.EmployeeId]);
      Width := 128;
      AutoSize := false;
      Alignment := taLeftJustify;
      Font.Size := 10;
      Align := alLeft;
      Parent := rowPanel;
      AlignWithMargins := True;
      Margins.Left := 0;
      Margins.Right := 0;
      Margins.Top := 5;
      Margins.Bottom := 0;
      Caption := employee.FullName;
    end;
    empValuesLabel := TLabel.Create(rowPanel);
    with empValuesLabel do
    begin
      Name := Format('Score%d_Values', [employee.EmployeeId]);
      Parent := rowPanel;
      Align := alClient;
      AutoSize := false;
      Alignment := taLeftJustify;
      Caption := '';
    end;
  end;
end;

procedure TForm1.UpdateScorecardPanel(const score: TEmployeeScore);
var
  valuesText: string;
  pn: TPanel;
  lbl: TLabel;
  panelName: string;
  labelname: string;
begin
  panelName := Format('PanelScorecards%d', [score.EmployeeId]);
  labelname := Format('Score%d_Values', [score.EmployeeId]);
  pn := scrboxScorecards.FindComponent(panelName) as TPanel;
  lbl := pn.FindComponent(labelname) as TLabel;
  valuesText := ValuesToStringPipes(score.OrderValues);
  lbl.Caption := Format('%d:  %s', [score.OrderCount, valuesText]);
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
  score: TEmployeeScore;
begin
  if lbxMonths.ItemIndex < 0 then
    Exit;
  if TryExtractMonthFromItem(lbxMonths.Items[lbxMonths.ItemIndex], yy, mm) then
  begin
    scores := fDataModule1.CalculateMonthlyScorecards(yy, mm);
    for score in scores do
    begin
      UpdateScorecardPanel(score)
    end;
  end;
end;

procedure TForm1.tmrStartTimer(Sender: TObject);
begin
  tmrStart.Enabled := False;
  BuildScorecardPanels(fEmployees);
end;

end.
