unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Controls.Presentation, FMX.ListView, FMX.Objects;

type
  TForm4 = class(TForm)
    ListView1: TListView;
    Button1: TButton;
    Button2: TButton;
    Switch1: TSwitch;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListView1PullRefresh(Sender: TObject);
    procedure Switch1Switch(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

/// <summary> выключаем отрисовку кастомного цвета у Item'a </summary>
procedure myDefaultItemColor(const aLV: TListView; const aItemIndex: integer;
  const aUseCustomColor: Boolean = false);
begin
  with aLV.Items[aItemIndex] do
  begin
    Data['aUseCustomColor'] := aUseCustomColor;
  end;
end;

/// <summary> устанавливаем кастомной цвет для Item'a </summary>
procedure myCustomItemColor(const aLV: TListView; const aItemIndex: integer;
  const aCustomColor: TAlphaColor; const aUseCustomColor: Boolean = true);
begin
  with aLV.Items[aItemIndex] do
  begin
    Data['aUseCustomColor'] := aUseCustomColor;
    Data['aCustomColor'] := aCustomColor;
  end;
end;

procedure TForm4.Button1Click(Sender: TObject);
var
  i: integer;
begin
  ListView1.BeginUpdate;
  Switch1Switch(nil);
  for i := 0 to 20 do
  begin
    with ListView1.Items.Add do
    begin
      Text := 'Item Random ' + i.ToString;
      Detail := 'Text Detail';
      ButtonText := 'Text Button';
      if i mod 3 = 0 then
        Purpose := TListItemPurpose.Header;
    end;
  end;
  ListView1.SeparatorLeftOffset := 40;
  ListView1.SeparatorRightOffset := 5;
  ListView1.EndUpdate;
end;

procedure TForm4.Button2Click(Sender: TObject);
var
  i: integer;
begin
  ListView1.BeginUpdate;
  i := 1;
  myCustomItemColor(ListView1, i, TAlphaColorF.Create(random(255) / 255,
    random(255) / 255, random(255) / 255, random(255) / 255).ToAlphaColor);
  // myDefaultItemColor(ListView1, i, false);
  ListView1.Resize;
  ListView1.EndUpdate;
end;

procedure TForm4.ListView1PullRefresh(Sender: TObject);
begin
  Button1Click(Sender);
end;

procedure TForm4.Switch1Switch(Sender: TObject);
begin
  if not Switch1.IsChecked then
  begin
    ListView1.SetColorItemSelected(TAlphaColorRec.Orangered);
    ListView1.SetColorItemFill(TAlphaColorRec.Whitesmoke);
    ListView1.SetColorItemFillAlt(TAlphaColorRec.Lightgrey);
    ListView1.SetColorBackground(TAlphaColorRec.Whitesmoke);
    ListView1.SetColorItemSeparator(TAlphaColorRec.Red);

    ListView1.SetColorText(TAlphaColorRec.Darkmagenta);
    ListView1.SetColorTextSelected(TAlphaColorRec.Blueviolet);
    ListView1.SetColorTextDetail(TAlphaColorRec.Darksalmon);

    ListView1.SetColorHeader(TAlphaColorRec.Crimson);
    ListView1.SetColorTextHeader(TAlphaColorRec.Whitesmoke);
    ListView1.SetColorTextHeaderShadow(TAlphaColorRec.grey);

    ListView1.SetColorPullRefresh(TAlphaColorRec.Lime);
    ListView1.SetColorPullRefresh(TAlphaColorRec.Limegreen);
    ListView1.SetColorStretchGlow(TAlphaColorRec.Limegreen);
  end
  else
  begin
    ListView1.SetColorItemSelected(TAlphaColorRec.Black);
    ListView1.SetColorItemFill($FF03246C);
    ListView1.SetColorItemFillAlt(TAlphaColorRec.Gray);
    ListView1.SetColorBackground(TAlphaColorRec.Darkgray);
    ListView1.SetColorItemSeparator(TAlphaColorRec.Blue);

    ListView1.SetColorText(TAlphaColorRec.Whitesmoke);
    ListView1.SetColorTextSelected(TAlphaColorRec.Lightgrey);
    ListView1.SetColorTextDetail(TAlphaColorRec.Lightgray);

    ListView1.SetColorHeader(TAlphaColorRec.Black);
    ListView1.SetColorTextHeader(TAlphaColorRec.Whitesmoke);
    ListView1.SetColorTextHeaderShadow(TAlphaColorRec.grey);

    ListView1.SetColorPullRefresh(TAlphaColorRec.Orange);
    ListView1.SetColorPullRefresh(TAlphaColorRec.Orangered);
    ListView1.SetColorStretchGlow(TAlphaColorRec.Orangered);
  end;
end;

end.
