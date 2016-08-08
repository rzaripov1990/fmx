unit UMyMsgDlg;

{
  author: krapotkin

  * modified by ZuBy, 2016
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UBaseDialog, FMX.Controls.Presentation, FMX.Objects, FMX.Layouts;

type
  TMyMsgDlg = class(TfBaseDialog)
    Layout1: TLayout;
    bOk: TSpeedButton;
    bCancel: TSpeedButton;
    Title: TText;
    Line1: TLine;
    procedure bOkClick(Sender: TObject);
  private
    aParentForm: TForm;
    OKProc: TThreadProcedure;
    OKHandler: TNotifyEvent;
    LikeAndroid5: boolean;
    { Private declarations }
    procedure Init(const aPrompt: string; AOwner: TComponent);
    procedure myResize;
  public
    constructor Create(const aPrompt: string; AOwner: TComponent;
      const AOKHandler: TNotifyEvent = nil;
      aLikeAndroid5: boolean = true); overload;
    constructor Create(const aPrompt: string; AOwner: TComponent;
      const AOKProc: TThreadProcedure = nil;
      aLikeAndroid5: boolean = true); overload;
    procedure ShowMe;
  end;

implementation

{$R *.fmx}

uses UGlobal;

{ TMyMsgDlg }

procedure TMyMsgDlg.bOkClick(Sender: TObject);
begin
  if Assigned(OKHandler) then
    OKHandler(Self);
  if Assigned(OKProc) then
    OKProc;
end;

procedure TMyMsgDlg.Init(const aPrompt: string; AOwner: TComponent);
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

{$IF defined (ANDROID)} Title.Font.Size := 15; {$ENDIF}
  Title.Text := aPrompt;

  bOk.Size.PlatformDefault := true;
  bCancel.Size.PlatformDefault := true;

  myResize;
end;

procedure TMyMsgDlg.myResize;
begin
  inherited Resize;
  if Assigned(aParentForm) then
  begin
    ClientHeight := aParentForm.ClientHeight;
    ClientWidth := aParentForm.ClientWidth;
  end;
  Position := TFormPosition.OwnerFormCenter;

  Layout1.Height := bOk.Height;
  r1.Width := ClientWidth - (ClientWidth * 0.15);
  r1.Height := 10 + r1.Padding.Top + Layout1.Height + Title.Height +
    Title.Margins.Bottom;

  bOk.Width := (Layout1.Width / 2) - 7.5;
  bCancel.Width := bOk.Width;

  case TOSVersion.Platform of
    pfWindows, pfMacOS:
      begin
        bOk.Align := TAlignLayout.Left;
        bCancel.Align := TAlignLayout.Right;
      end;
    pfAndroid:
      begin
        r1.Sides := [];
        r1.Stroke.Kind := TBrushKind.None;

        if TOSVersion.Check(5) and LikeAndroid5 then
        begin
          Line1.Visible := false;
          bOk.StyledSettings := [TStyledSetting.Family];
          bCancel.StyledSettings := [TStyledSetting.Family];
          bOk.Font.Style := [TFontStyle.fsBold];
          bCancel.Font.Style := [TFontStyle.fsBold];
          bOk.FontColor := $FF009688;
          bCancel.FontColor := $FF009688;
          // bOk.Font.StyleExt := TFontStyleExt.Create(TFontWeight.Semibold);
          bOk.Text := AnsiUpperCase(bOk.Text);
          bCancel.Text := AnsiUpperCase(bCancel.Text);

          bOk.Font.Size := 13;
          bCancel.Font.Size := 13;

          bOk.Align := TAlignLayout.MostRight;
          bCancel.Align := TAlignLayout.Right;

          Title.Margins.Bottom := 8;
          r1.Height := 8 + r1.Padding.Top + Layout1.Height + Title.Height +
            Title.Margins.Bottom;

          bOk.Margins.Top := (Layout1.Height - bOk.Height) / 2;
          bCancel.Margins.Top := (Layout1.Height - bCancel.Height) / 2;
          bOk.Margins.Right := 10;
          bOk.Margins.Left := 5;
          if bOk.Text.Length > 5 then
            bOk.Width := bOk.Canvas.TextWidth(bOk.Text) + 10
          else
            bOk.Width := (bOk.Canvas.TextWidth(bOk.Text) * 2) + 10;

          if bCancel.Text.Length > 5 then
            bCancel.Width := bCancel.Canvas.TextWidth(bCancel.Text) + 10
          else
            bCancel.Width := (bCancel.Canvas.TextWidth(bCancel.Text) * 2) + 10;
        end
        else
        begin
          bOk.Align := TAlignLayout.Right;
          bCancel.Align := TAlignLayout.Left;
        end;
      end;
  else
    begin
      bOk.Align := TAlignLayout.Right;
      bCancel.Align := TAlignLayout.Left;
    end;
  end;

  if Title.AutoSize then
  begin
    Title.Margins.Left := (r1.Width - Title.Width) / 2;
  end;
end;

procedure TMyMsgDlg.ShowMe;
begin
  myResize;
  Self.ShowModal(
    procedure(AResult: TModalResult)
    begin
    end);
end;

constructor TMyMsgDlg.Create(const aPrompt: string; AOwner: TComponent;
const AOKHandler: TNotifyEvent = nil; aLikeAndroid5: boolean = true);
begin
  inherited Create(AOwner);
  LikeAndroid5 := aLikeAndroid5;
  OKHandler := AOKHandler;
  Init(aPrompt, AOwner);
end;

constructor TMyMsgDlg.Create(const aPrompt: string; AOwner: TComponent;
const AOKProc: TThreadProcedure = nil; aLikeAndroid5: boolean = true);
begin
  inherited Create(AOwner);
  LikeAndroid5 := aLikeAndroid5;
  OKProc := AOKProc;
  Init(aPrompt, AOwner);
end;

end.
