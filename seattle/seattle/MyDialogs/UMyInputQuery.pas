unit UMyInputQuery;

{
  author: krapotkin

  * modified by ZuBy, 2016
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UBaseDialog, FMX.Controls.Presentation, FMX.Objects, FMX.Layouts, FMX.Edit,
  System.Generics.Collections;

type
  TMyInputQuery = class(TfBaseDialog)
    bOk: TSpeedButton;
    Layout1: TLayout;
    bCancel: TSpeedButton;
    Line1: TLine;
    procedure bOkClick(Sender: TObject);
    procedure LimitText(Sender: TObject);
  private
    FHeightSums: Single;
    aParentForm: TForm;
    OKProc: TThreadProcedure;
    OKHandler: TNotifyEvent;
    function GetPrompt(index: integer): string;
    function GetValue(index: integer): string;
    procedure SetPrompt(index: integer; const Value: string);
    procedure SetValue(index: integer; const Value: string);
    procedure Init(const APrompt, AValue: TArray<string>; AOwner: TComponent);
    procedure myResize;
  public
    Labels: TObjectList<TLabel>;
    Edits: TObjectList<TEdit>;
    property Prompt[index: integer]: string read GetPrompt write SetPrompt;
    property Values[index: integer]: string read GetValue write SetValue;
    constructor Create(const APrompt, AValue: TArray<string>; AOwner: TComponent;
      const AOKHandler: TNotifyEvent); overload;
    constructor Create(const APrompt, AValue: TArray<string>; AOwner: TComponent;
      const AOKProc: TThreadProcedure); overload;
    destructor Destroy; override;
    procedure ShowMe;
  end;

implementation

{$R *.fmx}

uses UGlobal;

{ TMyInputQuery }

procedure TMyInputQuery.bOkClick(Sender: TObject);
begin
  if Assigned(OKHandler) then
    OKHandler(Self);
  if Assigned(OKProc) then
    OKProc;
end;

procedure TMyInputQuery.Init(const APrompt, AValue: TArray<string>; AOwner: TComponent);
var
  aLabel: TLabel;
  aEdit: TEdit;
  i: integer;
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

  Labels := TObjectList<TLabel>.Create(true);
  Edits := TObjectList<TEdit>.Create(true);
  for i := High(APrompt) downto 0 do
  begin
    aEdit := TEdit.Create(Self);
    aEdit.Parent := r1;
    if i < length(AValue) then
      aEdit.Text := AValue[i];
    aEdit.Align := TAlignLayout.Top;
    aEdit.Margins.Left := 5;
    aEdit.Margins.Right := 5;
    aEdit.OnChangeTracking := LimitText;
    Edits.Insert(0, aEdit);

    aLabel := TLabel.Create(Self);
    aLabel.Parent := r1;
    aLabel.TextSettings.WordWrap := true;
    aLabel.AutoSize := true;
    aLabel.TextAlign := TTextAlign.Center;
    aLabel.Align := TAlignLayout.Top;
    aLabel.Margins.Left := 5;
    aLabel.Margins.Right := 5;
    aLabel.Text := APrompt[i];
    Labels.Insert(0, aLabel);
  end;
  Realign;
  FHeightSums := 0;
  for i := High(APrompt) downto 0 do
    FHeightSums := FHeightSums + Labels[i].Height + Edits[i].Height;
  ActiveControl := Edits[0];

  bOk.Size.PlatformDefault := true;
  bCancel.Size.PlatformDefault := true;
  myResize;
end;

procedure TMyInputQuery.LimitText(Sender: TObject);
begin
{$IF not defined (MSWINDOWS)}
  if (Sender as TEdit).MaxLength > 0 then
  begin
    if (Sender as TEdit).Text.length >= (Sender as TEdit).MaxLength then
      (Sender as TEdit).Text := (Sender as TEdit).Text.Substring(0, (Sender as TEdit).MaxLength);
  end;
{$ENDIF}
end;

procedure TMyInputQuery.myResize;
begin
  inherited Resize;
  if Assigned(aParentForm) then
  begin
    ClientHeight := aParentForm.ClientHeight;
    ClientWidth := aParentForm.ClientWidth;
  end;
  Position := TFormPosition.OwnerFormCenter;

  r1.Sides := [];
  r1.Stroke.Kind := TBrushKind.None;
  bOk.Align := TAlignLayout.Right;
  bCancel.Align := TAlignLayout.Left;

  case TOSVersion.Platform of
    pfWindows, pfMacOS:
      begin
        bOk.Align := TAlignLayout.Left;
        bCancel.Align := TAlignLayout.Right;
      end;
    pfiOS:
      begin
        r1.Sides := [TSide.Left, TSide.Top, TSide.Right, TSide.Bottom];
        r1.Stroke.Kind := TBrushKind.Solid;
      end;
  end;

  Layout1.Height := bOk.Height;
  r1.Width := ClientWidth - (ClientWidth * 0.15);
  r1.Height := 10 + r1.Padding.Top + Layout1.Height + FHeightSums;

  bOk.Width := (Layout1.Width / 2) - 7.5;
  bCancel.Width := bOk.Width;
end;

constructor TMyInputQuery.Create(const APrompt, AValue: TArray<string>; AOwner: TComponent;
  const AOKHandler: TNotifyEvent);
begin
  inherited Create(AOwner);
  OKHandler := AOKHandler;
  Init(APrompt, AValue, AOwner);
end;

constructor TMyInputQuery.Create(const APrompt, AValue: TArray<string>; AOwner: TComponent;
  const AOKProc: TThreadProcedure);
begin
  inherited Create(AOwner);
  OKProc := AOKProc;
  Init(APrompt, AValue, AOwner);
end;

destructor TMyInputQuery.Destroy;
begin
  FreeAndNil(Labels);
  FreeAndNil(Edits);
  inherited;
end;

function TMyInputQuery.GetPrompt(index: integer): string;
begin
  result := Labels[index].Text;
end;

function TMyInputQuery.GetValue(index: integer): string;
begin
  if index < Edits.Count then
    result := Edits[index].Text
  else
    result := '';
end;

procedure TMyInputQuery.SetPrompt(index: integer; const Value: string);
begin
  Labels[index].Text := Value;
end;

procedure TMyInputQuery.SetValue(index: integer; const Value: string);
begin
  Edits[index].Text := Value;
end;

procedure TMyInputQuery.ShowMe;
begin
  Self.ShowModal(
    procedure(AResult: TModalResult)
    begin
    end);
end;

end.
