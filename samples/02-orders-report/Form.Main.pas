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
  Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    procedure TestMonths;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Spring,
  Spring.Collections,

  Data.DataModule1;


procedure TForm1.TestMonths();
var
  aListBox: TListBox;
  activeMonths: IList<Tuple<String, Word, Word>>;
  aMonth: Tuple<String, Word, Word>;
begin
  aListBox := TListBox.Create(self);
  aListBox.Align := alLeft;
  aListBox.AlignWithMargins := True;
  aListBox.Parent := Self;
  DataModule1.Connect();
  activeMonths := DataModule1.GetActiveMonths();
  aListBox.Clear;
  for aMonth in activeMonths do
  begin
    aListBox.AddItem(aMonth.Value1,nil);
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  TestMonths();
end;

end.
