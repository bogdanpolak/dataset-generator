program OrdersReport;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {Form1},
  Logic.Scorecards in 'Logic.Scorecards.pas',
  Data.DataModule1 in 'Data.DataModule1.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
