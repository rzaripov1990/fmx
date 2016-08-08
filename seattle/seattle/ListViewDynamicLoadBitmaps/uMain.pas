unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.StdCtrls, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo;

type
  TForm16 = class(TForm)
    ListView1: TListView;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure ListView1Paint(Sender: TObject; Canvas: TCanvas;
      const [Ref] ARect: TRectF);
    procedure ListView1UpdateObjects(const Sender: TObject;
      const AItem: TListViewItem);
  private
    { Private declarations }
    procedure LoadBitmaps(const i: integer);
  public
    { Public declarations }
  end;

var
  Form16: TForm16;

implementation

{$R *.fmx}

uses Math, FMX.Bitmap.Helpers;

procedure TForm16.Button2Click(Sender: TObject);
var
  sUrl: string;
  i: integer;
begin
  for i := 1 to 400 do
  begin
    sUrl := 'http://fire-monkey.ru/uploads/profile/photo-' + i.ToString
      + '.jpg';

    with ListView1.Items.Add do
    begin
      Text := sUrl;
      Data['loading'] := 0; // даём знать, что можно загрузить картинку
    end;
  end;
end;

procedure TForm16.ListView1Paint(Sender: TObject; Canvas: TCanvas;
  const [Ref] ARect: TRectF);
var
  i: integer;
begin
  for i := ListView1.getFirstVisibleItemIndex to ListView1.
    getFirstVisibleItemIndex + ListView1.getVisibleCount do
    LoadBitmaps(i);
end;

procedure TForm16.ListView1UpdateObjects(const Sender: TObject;
  const AItem: TListViewItem);
begin
  AItem.Height := 90 + random(20); // для наглядности
end;

procedure TForm16.LoadBitmaps(const i: integer);
begin
  if (ListView1.Items.Count > 0) then
  begin
    if (i >= 0) and (i < ListView1.Items.Count) then
      if (ListView1.Items[i].Bitmap.IsEmpty) and
        (ListView1.Items[i].Data['loading'].AsInteger = 0) then
      begin
        ListView1.Items[i].Data['loading'] := 1;
        ListView1.Items[i].Bitmap.LoadFromUrl(ListView1.Items[i].Text,
          'http://i1.wp.com/fire-monkey.ru/uploads/set_resources_1/84c1e40ea0e759e3f1505eb1788ddf3c_default_photo.png');
      end;
  end;
end;

end.
