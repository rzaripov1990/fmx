unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FGX.ActionSheet, FGX.ActionSheet.Types,
  FMX.Pickers, FMX.Objects;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Rectangle1: TRectangle;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure PickerListClick(Sender: TObject; const AValueIndex: Integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  FPickerListValues: TArray<string>;

implementation

{$R *.fmx}

uses
  FMX.Pickers.Helper;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Setlength(FPickerListValues, 3);
  FPickerListValues[0] := 'aaa';
  FPickerListValues[1] := 'bbb';
  FPickerListValues[2] := 'ccc';
  TmyPicker.ShowList(FPickerListValues, Button1, PickerListClick);
end;

procedure TForm1.PickerListClick(Sender: TObject; const AValueIndex: Integer);
begin
  ShowMessage(FPickerListValues[AValueIndex]);
end;

end.
