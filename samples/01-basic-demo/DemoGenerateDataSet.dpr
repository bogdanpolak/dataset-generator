program DemoGenerateDataSet;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {FormMain},
  Comp.Generator.DataSetCode in '..\..\src\Comp.Generator.DataSetCode.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
