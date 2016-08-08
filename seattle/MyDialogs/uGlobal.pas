unit UGlobal;

{
  author: krapotkin
}

interface

uses
  System.Types, FMX.Forms, FMX.Types, FMX.TextLayout, FMX.Graphics, System.Math;

function GetParentForm(O: TFmxObject): TForm;

var
  TextLayout: TTextLayout;

function TextHeight(const AText: string; const aWidth: Single; const ATextSettings: TTextSettings): Single;

implementation

function TextHeight(const AText: string; const aWidth: Single; const ATextSettings: TTextSettings): Single;
// uses FMX.TextLayout, FMX.Graphics, System.Math
begin
  Result := 0;
  if AText = '' then
    Exit;

  TextLayout.BeginUpdate;
  try
    TextLayout.Text := AText;
    TextLayout.MaxSize := TPointF.Create(aWidth, 1000);
    TextLayout.Font := ATextSettings.Font;
    TextLayout.WordWrap := ATextSettings.WordWrap; // or (Pos('#13#10', AText) > 0) or (AText.Length > 255);
    TextLayout.Trimming := ATextSettings.Trimming;
    TextLayout.HorizontalAlign := ATextSettings.HorzAlign;
    TextLayout.VerticalAlign := ATextSettings.VertAlign;
  finally
    TextLayout.EndUpdate;
  end;

  Result := Round(TextLayout.Height);
end;

function GetParentForm(O: TFmxObject): TForm;
var
  P: TFmxObject;
begin
  Result := nil;
  P := O.Parent;
  while (P <> nil) and (not(P is TForm)) do
    P := P.Parent;
  if P <> nil then
    Result := P as TForm;
end;

initialization

TextLayout := TTextLayoutManager.DefaultTextLayout.Create;

finalization

TextLayout.Free;

end.
