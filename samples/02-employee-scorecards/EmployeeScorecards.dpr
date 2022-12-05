program EmployeeScorecards;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {Form1},
  Data.DataModule1 in 'Data.DataModule1.pas' {DataModule1: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
