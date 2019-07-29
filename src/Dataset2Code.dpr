program Dataset2Code;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {Form1},
  Action.CreateMemTable in 'Action.CreateMemTable.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
