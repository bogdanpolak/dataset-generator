unit Form.Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Actions, Vcl.ActnList,
  {-}
  Logic.Scorecards;

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
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbxMonthsClick(Sender: TObject);
  private
    procedure FillListBoxWithMonths(const aListBox: TListBox);
    procedure ShowData(const aScorecards: TScorecards);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Spring,
  Spring.Collections,
  {-}
  Data.DataModule1;


procedure TForm1.actDatabaseConnectExecute(Sender: TObject);
begin
  DataModule1.Connect();
  FillListBoxWithMonths(lbxMonths);
end;

procedure TForm1.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  gboxScorecards.Visible := DataModule1.IsConnected();
  actDatabaseConnect.Enabled := not DataModule1.IsConnected();
end;

procedure TForm1.FillListBoxWithMonths(const aListBox: TListBox);
var
  activeMonths: IList<Tuple<String, Word, Word>>;
  aMonth: Tuple<String, Word, Word>;
begin
  activeMonths := DataModule1.GetActiveMonths();
  aListBox.Clear;
  for aMonth in activeMonths do
  begin
    aListBox.Items.Insert(0, aMonth.Value1);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  gboxScorecards.Visible := False;
  gboxScorecards.Align := alClient;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  actDatabaseConnect.Execute;
end;

procedure TForm1.ShowData(const aScorecards: TScorecards);
var
  employeeScore: TEmployeeScore;
begin
  MemoTest.Clear;
  for employeeScore in aScorecards.fEmployeeScores.Values do
  begin
    MemoTest.Lines.Add(Format('%s (%d) - %d orders',
    [employeeScore.fEmployeeName, employeeScore.fEmployeeId, employeeScore.fOrderCount]));
  end;
end;

procedure TForm1.lbxMonthsClick(Sender: TObject);
var
  strMonth: string;
  aYear: word;
  aMonth: word;
  aScorecards: TScorecards;
begin
  if lbxMonths.ItemIndex<0 then
    Exit;
  strMonth := lbxMonths.Items[lbxMonths.ItemIndex];
  aYear := strMonth.Substring(0,4).ToInteger();
  aMonth := strMonth.Substring(5,2).ToInteger();
  aScorecards := TScorecards.Create(aYear, aMonth);
  ShowData(aScorecards);
end;

end.
