unit LVHelper;

{
  author: ZuBy


  Delphi XE8 = TCustomListView
  Delphi 10  = TListViewBase

  For Delphi XE8, compiler version 29.0 has VER290 defined.
}

interface

uses FMX.ListView, System.UITypes;

type
  TListViewHelper = class helper for {$IF CompilerVersion > 29.0} TListViewBase {$ELSE} TCustomListView {$ENDIF}
  // Item
  procedure SetColorItemSelected(aColor: TAlphaColor);
procedure SetColorBackground(aColor: TAlphaColor);
procedure SetColorItemFill(aColor: TAlphaColor);
procedure SetColorItemFillAlt(aColor: TAlphaColor);
procedure SetColorItemSeparator(aColor: TAlphaColor);

// Text
procedure SetColorText(aColor: TAlphaColor);
procedure SetColorTextSelected(aColor: TAlphaColor);
procedure SetColorTextDetail(aColor: TAlphaColor);
procedure SetColorTextHeader(aColor: TAlphaColor);
procedure SetColorTextHeaderShadow(aColor: TAlphaColor);
procedure SetColorButtonText(aColor: TAlphaColor);
procedure SetColorButtonTextPressed(aColor: TAlphaColor);
procedure SetColorDeleteText(aColor: TAlphaColor);
procedure SetColorDeleteTintColor(aColor: TAlphaColor);
procedure SetColorDeleteTextPressed(aColor: TAlphaColor);

function GetScrollWidth: Single;
  end;

implementation

{ TListViewHelper }

uses FMX.Styles.Objects, FMX.Objects;

function TListViewHelper.GetScrollWidth: Single;
const
{$IFDEF IOS}
  DefaultScrollBarWidth = 7;
{$ELSE}
{$IFDEF MACOS}
  DefaultScrollBarWidth = 7;
{$ENDIF}
{$ENDIF}
{$IFDEF MSWINDOWS}
  DefaultScrollBarWidth = 16;
{$ENDIF}
{$IFDEF ANDROID}
  DefaultScrollBarWidth = 7;
{$ENDIF}
begin
  Result := DefaultScrollBarWidth;
end;

procedure TListViewHelper.SetColorBackground(aColor: TAlphaColor);
begin
{$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FBackgroundStyleColor := aColor;
end;

procedure TListViewHelper.SetColorButtonText(aColor: TAlphaColor);
begin
{$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FStyleResources.ButtonTextColor := aColor;
end;

procedure TListViewHelper.SetColorButtonTextPressed(aColor: TAlphaColor);
begin
{$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FStyleResources.ButtonTextPressedColor := aColor;
end;

procedure TListViewHelper.SetColorDeleteTintColor(aColor: TAlphaColor);
begin
{$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FdeleteButton.TintColor := aColor;
end;

procedure TListViewHelper.SetColorDeleteText(aColor: TAlphaColor);
begin
  // {$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FStyleResources.ButtonDeleteItemStyleImage.Normal := nil;
{$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FStyleResources.DeleteButtonTextColor := aColor;
end;

procedure TListViewHelper.SetColorDeleteTextPressed(aColor: TAlphaColor);
begin
  // {$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FStyleResources.ButtonDeleteItemStyleImage.Pressed := nil;
{$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FStyleResources.DeleteButtonTextPressedColor := aColor;
end;

procedure TListViewHelper.SetColorItemFill(aColor: TAlphaColor);
begin
{$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FItemStyleFillColor := aColor;
end;

procedure TListViewHelper.SetColorItemFillAlt(aColor: TAlphaColor);
begin
{$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FItemStyleFillAltColor := aColor;
end;

procedure TListViewHelper.SetColorItemSeparator(aColor: TAlphaColor);
begin
{$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FItemStyleFrameColor := aColor;
end;

procedure TListViewHelper.SetColorText(aColor: TAlphaColor);
begin
{$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FStyleResources.DefaultTextColor := aColor;
end;

procedure TListViewHelper.SetColorTextDetail(aColor: TAlphaColor);
begin
{$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FStyleResources.DetailTextColor := aColor;
end;

procedure TListViewHelper.SetColorTextHeader(aColor: TAlphaColor);
begin
{$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FStyleResources.HeaderTextColor := aColor;
end;

procedure TListViewHelper.SetColorTextHeaderShadow(aColor: TAlphaColor);
begin
{$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FStyleResources.HeaderTextShadowColor := aColor;
end;

procedure TListViewHelper.SetColorTextSelected(aColor: TAlphaColor);
begin
{$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FStyleResources.DefaultTextSelectedColor := aColor;
end;

procedure TListViewHelper.SetColorItemSelected(aColor: TAlphaColor);
begin
{$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FSelectionStyleImage := nil;
{$IF CompilerVersion > 29.0}TListViewBase{$ELSE}TCustomListView{$ENDIF}(self).FSelectionStyleColor := aColor;
end;

end.
