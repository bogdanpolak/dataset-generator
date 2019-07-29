program Dataset2Code;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {FormMain},
  Action.CreateMemTable in 'Action.CreateMemTable.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
