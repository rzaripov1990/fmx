unit FMX.Pickers.Helper;

interface

uses
  System.Types, System.SysUtils,
  FMX.Types, FMX.Forms, FMX.Controls, FMX.Platform, FMX.Pickers;

type
  TmyPicker = record
    class procedure ShowList(const aValues: TArray<string>; const aParent: TControl; aEvent: TOnValueChanged;
      const aIndex: integer = -1); static;
  end;

implementation

{ TmyPicker }

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

class procedure TmyPicker.ShowList(const aValues: TArray<string>; const aParent: TControl; aEvent: TOnValueChanged;
  const aIndex: integer = -1);
var
  aPickerService: IFMXPickerService;
  aPicker: TCustomListPicker;
  i: integer;
{$IFDEF IOS}
  aWidth: Single;
  aForm: TForm;
{$ENDIF}
begin
  if aParent = nil then
    exit;

  if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, aPickerService) then
  begin
    aPicker := aPickerService.CreateListPicker;
{$IFDEF IOS}
    aForm := GetParentForm(aParent);
    aWidth := aForm.Width / 2;
    aPicker.Parent := nil;
    aPicker.AbsoluteTargetRect := RectF((aWidth + aForm.Width) / 2, 0, aForm.Width, aForm.Height);
{$ELSE}
    aPicker.Parent := aParent;
{$ENDIF}
    aPicker.OnValueChanged := aEvent;
    for i := Low(aValues) to High(aValues) do
      aPicker.Values.Add(aValues[i]);
    aPicker.ItemIndex := aIndex;
    aPicker.Show;
  end;
end;

end.
