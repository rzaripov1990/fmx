unit UMyToast;

{
  author: krapotkin (TFBaseDialog)

  * modified by ZuBy, 2016
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UBaseDialog, FMX.Controls.Presentation, FMX.Objects, FMX.Layouts;

type
  TMyToast = class(TfBaseDialog)
    Title: TText;
  private
    aTimer: TTimer;
    aParentForm: TForm;
    { Private declarations }
    procedure Init(const AText: string; AOwner: TComponent; const BottomOffset: Single;
      const aBackgroundColor: TAlphaColor; const aTextColor: TAlphaColor);
    procedure myResize;
    procedure OnTimer(Sender: TObject);
  public
    constructor Create(const AText: string; AOwner: TComponent; const ADuration: Integer;
      const BottomOffset: Single = 0; const aBackgroundColor: TAlphaColor = TAlphaColorRec.Null;
      const aTextColor: TAlphaColor = TAlphaColorRec.Black); overload;
    procedure ShowMe;
  end;

implementation

{$R *.fmx}

uses UGlobal;

{ TMyToast }

procedure TMyToast.Init(const AText: string; AOwner: TComponent; const BottomOffset: Single;
  const aBackgroundColor: TAlphaColor; const aTextColor: TAlphaColor);
begin
  if AOwner <> nil then
  begin
    if AOwner is TForm then
      aParentForm := AOwner as TForm
    else
      aParentForm := GetParentForm(AOwner as TFmxObject);
    StyleBook := aParentForm.StyleBook;
  end;
  if aParentForm = nil then
    aParentForm := TForm(Screen.ActiveForm);

  r1.Sides := [];
  r1.Stroke.Kind := TBrushKind.None;
  Self.Name := 'MyToast' + FormatDateTime('dd_mm_yyyy_hhnnsszzz', now);

{$IF defined (ANDROID)} Title.Font.Size := 15; {$ENDIF}
  Title.TextSettings.VertAlign := TTextAlign.Leading;
  Title.TextSettings.HorzAlign := TTextAlign.Leading;

  r1.Margins.Left := (ClientWidth - (ClientWidth * 0.8)) / 2;
  r1.Margins.Right := r1.Margins.Left;
  r1.Height := TextHeight(AText, Title.Width - (Title.Margins.Left + Title.Margins.Right), Title.TextSettings) +
    Title.Margins.Top + Title.Margins.Bottom + 20;
  r1.Padding.Top := 0;
  r1.Margins.Bottom := BottomOffset;
  r1.Fill.Color := aBackgroundColor;

  r2.Fill.Color := TAlphaColorRec.Null;

  Title.TextSettings.VertAlign := TTextAlign.Center;
  Title.TextSettings.HorzAlign := TTextAlign.Center;
  Title.Color := aTextColor;
  Title.Text := AText;

  myResize;
end;

procedure TMyToast.myResize;
begin
  inherited Resize;
  if Assigned(aParentForm) then
  begin
    ClientHeight := aParentForm.ClientHeight;
    ClientWidth := aParentForm.ClientWidth;
  end;
  Position := TFormPosition.OwnerFormCenter;
end;

procedure TMyToast.OnTimer(Sender: TObject);
begin
  aTimer.Enabled := false;
  Self.Close;
end;

procedure TMyToast.ShowMe;
begin
  myResize;
  Self.ShowModal(
    procedure(AResult: TModalResult)
    begin
    end);
end;

constructor TMyToast.Create(const AText: string; AOwner: TComponent; const ADuration: Integer;
const BottomOffset: Single = 0; const aBackgroundColor: TAlphaColor = TAlphaColorRec.Null;
const aTextColor: TAlphaColor = TAlphaColorRec.Black);
begin
  inherited Create(AOwner);
  aTimer := TTimer.Create(Self);
  aTimer.Interval := ADuration;
  aTimer.OnTimer := OnTimer;
  aTimer.Enabled := true;
  Init(AText, AOwner, BottomOffset, aBackgroundColor, aTextColor);
end;

end.
