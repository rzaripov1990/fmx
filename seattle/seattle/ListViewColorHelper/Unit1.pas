unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListView;

type
  TForm1 = class(TForm)
    ListView1: TListView;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses LVHelper;

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
begin
  ListView1.SetColorItemSelected(TAlphaColorRec.Orangered);
  ListView1.SetColorItemFill(TAlphaColorRec.Gray);
  ListView1.SetColorItemFillAlt(TAlphaColorRec.Lightgrey);
  ListView1.SetColorBackground(TAlphaColorRec.Black);
  ListView1.SetColorItemSeparator(TAlphaColorRec.Lightgray);

  ListView1.SetColorText(TAlphaColorRec.Red);
  ListView1.SetColorTextSelected(TAlphaColorRec.White);
  ListView1.SetColorTextDetail(TAlphaColorRec.Yellow);
  ListView1.SetColorTextDetail(TAlphaColorRec.Yellow);

  ListView1.SetColorTextHeader(TAlphaColorRec.Green);
  ListView1.SetColorTextHeaderShadow(TAlphaColorRec.Lightgray);

  ListView1.SetColorButtonText(TAlphaColorRec.Orange);
  ListView1.SetColorButtonTextPressed(TAlphaColorRec.Orangered);

  {ListView1.SetColorDeleteText(TAlphaColorRec.Aqua);
  ListView1.SetColorDeleteTextPressed(TAlphaColorRec.Rosybrown);}

  Caption := 'LV Scroll Width =' + ListView1.GetScrollWidth.ToString;

  for I := 0 to 10 do
  begin
    with ListView1.Items.Add do
    begin
      if I = random(10) - 1 then
      begin
        Purpose := TListItemPurpose.Header;
        Text := 'Header ' + I.ToString;
      end
      else
      begin
        Text := 'Item ' + I.ToString;
        Detail := 'Detail Info';

        // if I = random(10) - 1 then
        ButtonText := 'Button';
      end;
    end;
  end;
end;

end.
