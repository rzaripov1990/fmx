unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.ListView, FMX.Objects, FMX.ScrollBox, FMX.Gestures, FMX.Layouts, System.ImageList, FMX.ImgList,
  FMX.ListView.Horz;

type
  TFormGP = class(TForm)
    StyleBook1: TStyleBook;
    PresentedScrollBox1: TPresentedScrollBox;
    Layout1: TLayout;
    Label1: TLabel;
    ImageList1: TImageList;
    Layout2: TLayout;
    Label2: TLabel;
    Rectangle1: TRectangle;
    Label3: TLabel;
    ListViewHorz1: TListViewHorz;
    ListViewHorz2: TListViewHorz;
    GestureManager1: TGestureManager;
    procedure ListView1Gesture(Sender: TObject; const [Ref] EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure ListView1MouseLeave(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ListViewHorz1UpdateObjects(const Sender: TObject; const AItem: TListViewItem);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormGP: TFormGP;
  gPos: TPointF;

implementation

{$R *.fmx}

const
  aTitle: array [0 .. 8] of string = ('ПИКСЕЛИ', 'The Martian', 'Третий лишний 2', 'Прогулка', 'Hitman: Agent 47',
    'Миссия Невыполнима', 'The Perfect Guy', 'Такие разные близнецы', 'Мир Юрского Периода');
  aCurrency: array [0 .. 8] of string = ('360,00', '350,00', '730,00', '360,00', '350,00', '730,00', '360,00', '910,00',
    '730,00');

  aTitle2: array [0 .. 8] of string = ('Пятьдесят оттенков серого', 'Униженные и оскорбленные', 'Герой нашего времени',
    'Стихотворения', 'На пятьдесят оттенков темнее', 'Народные русские сказки', 'Бедные люди', 'Записки сумасшедшего',
    'Дубровский');
  aDetail2: array [0 .. 8] of string = ('Э. Джеймс', 'Федор Достоевский', 'Михаил Лермонтов', 'Александр Пушкин', 'Э. Джеймс',
    'Александр Афанасьев', 'Федор Достоевский', 'Николай Гоголь', 'Александр Пушкин');
  aCurrency2: array [0 .. 8] of string = ('994,18', '4,30', '4,30', '4,30', '994,18', '4,30', '4,30', '4,30', '4,30');

procedure TFormGP.FormActivate(Sender: TObject);
var
  I: integer;
begin
  ListViewHorz1.ItemAppearance.ItemHeight := 190;
  for I := 0 to 8 do
  begin
    with ListViewHorz1.Items.Add do
    begin
      ImageIndex := I;
      Data['myText'] := aCurrency[I] + ' ₸';
      Text := aTitle[I];
      Detail := 'Приключения и боевики';
    end;
  end;

  ListViewHorz2.ItemAppearance.ItemHeight := 190;
  for I := 0 to 8 do
  begin
    with ListViewHorz2.Items.Add do
    begin
      ImageIndex := 9 + I;
      Data['myText'] := aCurrency2[I] + ' ₸';
      Text := aTitle2[I];
      Detail := aDetail2[I];
    end;
  end;
end;

procedure TFormGP.ListView1Gesture(Sender: TObject; const [Ref] EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  dX, dY: Single;
  curViewPos: TPointF;
begin
  if EventInfo.GestureID = igiPan then
  begin
    if (not(TInteractiveGestureFlag.gfBegin in EventInfo.Flags)) then
    begin
      curViewPos := PresentedScrollBox1.ViewportPosition;
      dX := curViewPos.X + (gPos.X - EventInfo.Location.X);
      dY := curViewPos.Y + (gPos.Y - EventInfo.Location.Y);

      dX := 0;
      if dY < 0 then
        dY := 0;
      if dY > PresentedScrollBox1.ContentBounds.Height then
        dY := PresentedScrollBox1.ContentBounds.Height;

      PresentedScrollBox1.ViewportPosition := PointF(dX, dY);
    end;
    gPos := EventInfo.Location;
  end;
end;

procedure TFormGP.ListView1MouseLeave(Sender: TObject);
begin
  if (Sender as TListViewHorz).Items.Count > 0 then
    (Sender as TListViewHorz).ItemIndex := -1;
end;

procedure TFormGP.ListViewHorz1UpdateObjects(const Sender: TObject; const AItem: TListViewItem);
var
  myText: TListItemText;
begin
  myText := AItem.Objects.FindDrawable('myText') as TListItemText;
  if myText = nil then
  begin
    myText := TListItemText.Create(AItem);
    myText.Name := 'myText';
  end;

  myText.Align := TListItemAlign.Trailing;
  myText.TextAlign := TTextAlign.Trailing;
  myText.VertAlign := TListItemAlign.Trailing;
  myText.TextColor := TAlphaColorRec.Firebrick;
  myText.Width := 60;
  myText.Height := 20;
  myText.PlaceOffset.X := -(Sender as TListViewHorz).SideSpace;
  myText.PlaceOffset.Y := 0;
end;

end.
