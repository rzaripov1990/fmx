unit vkbdhelper;

{
  Force focused control visible when Android/IOS Virtual Keyboard showed or hiden
  How to use:
  place vkdbhelper into your project uses section. No more code needed.

  Changes by kami
  =======
  2016.02.29
  * fix issue with double-raised Keyboard_Show event, when FocusedControl.AbsoluteRect
  is already "upped" by own TLayout.

  2016.02.28
  * fix issue with Form.Fill vanish after AdjustByLayout applied.
  * disable not needed (???) timer proc.
  * improve own TLayout detection.
  * some refactoring

  Known issues:
  * after device rotate keyboard can change height without form notification.
  this can move focused control to wrong y-coord...
  * sometimes keyboard_show notification become with wrong size calc.
  Especially after device rotation (hide keyboard-rotate device-show keyboard). Why??? I don`t know...
  But standart demo app (Scrollable form demo) has same behavior.
  =======
  2016.01.26
  * fix issue with wrong VK coordinates calculation. Remove VKOffset.

  Changes by ZuBy
  ======= =======
  2016.01.24
  * clean Uses section
  * top margin value for Virtual Keyboard (global var VKOffset := [integer])
  * now cross-platform (IOS, Android)

  Changes
  =======
  2015.7.12
  * Fix space after hide ime and rotate
  * Fix rotate detection

}
interface

implementation

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.Types,
  System.Messaging,
  FMX.Types,
  FMX.Controls,
  FMX.Layouts,
  FMX.Objects,
  FMX.Forms;

type
  TVKStateHandler = class(TComponent)
  protected
    FVKMsgId: Integer;
    FSizeMsgId: Integer;
    FLastControl: TControl;

    FLastMargin: TPointF;
    FLastAlign: TAlignLayout;
    FLastBounds: TRectF;

    FVKVisibleTimer: TTimer;
    FLayoutTag: Integer;

    procedure DoVKVisibleChanged(const Sender: TObject; const Msg: System.Messaging.TMessage);
    procedure DoSizeChanged(const Sender: TObject; const Msg: System.Messaging.TMessage);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoVKVisibleCheck(ASender: TObject);
    procedure EnableVKCheck(AEnabled: Boolean);
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
  end;

var
  VKHandler: TVKStateHandler;
  FglVKB: TRectF;

const
  sVKBHelperLayout = 'ltVKBHelperLayout';

function IsVKVisible: Boolean;
begin
  Result := not FglVKB.IsEmpty;
end;

function GetVKBounds(var ARect: TRect; NewFormSize: TSize): Boolean;
var
  fm: TCommonCustomForm;
  ContentRect: TRectF;
begin
  Result := IsVKVisible;
  if Result then
    begin
      fm := Screen.ActiveForm;
      FglVKB.Width := NewFormSize.Width;
      FglVKB.Offset(0, { fm.top + } NewFormSize.height - FglVKB.bottom);
      ContentRect.TopLeft := fm.ClientToScreen(FglVKB.TopLeft);
      ContentRect.BottomRight := fm.ClientToScreen(FglVKB.BottomRight);
      ARect := ContentRect.Truncate;
    end;
end;

function ControlIsBackground(AChild, AParent: TFmxObject): Boolean;
begin
  Result := False;
  if not(AParent is TCustomForm) then
    Exit;
  if not(AChild is TRectangle) then
    Exit;
  if (TRectangle(AChild).Align = TAlignLayout.Contents) then
    Result := true;
end;

function FindCommonLayout(AParent: TFmxObject): TLayout;
begin
  Result := nil;
  if not Assigned(AParent) then
    Exit;
  if AParent.ChildrenCount = 0 then
    Exit;
  if (AParent.ChildrenCount = 1) and (AParent.Children[0] is TLayout) then
    Result := TLayout(AParent.Children[0]);

  if (AParent.ChildrenCount = 2) then
    begin
      if ControlIsBackground(AParent.Children[0], AParent) and (AParent.Children[1] is TLayout) then
        Result := TLayout(AParent.Children[1]);
      if ControlIsBackground(AParent.Children[1], AParent) and (AParent.Children[0] is TLayout) then
        Result := TLayout(AParent.Children[0]);
    end;

  if Assigned(Result) then
    if not StartsStr(sVKBHelperLayout, Result.Name) then // это не "общий" Layout
      Result := nil;
end;

{ TVKStateHandler }
constructor TVKStateHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVKMsgId := TMessageManager.DefaultManager.SubscribeToMessage(TVKStateChangeMessage, DoVKVisibleChanged);
  FSizeMsgId := TMessageManager.DefaultManager.SubscribeToMessage(TSizeChangedMessage, DoSizeChanged);
  FVKVisibleTimer := TTimer.Create(Self);
  FVKVisibleTimer.Enabled := False;
  FVKVisibleTimer.Interval := 100;
  FVKVisibleTimer.OnTimer := DoVKVisibleCheck;
end;

destructor TVKStateHandler.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TVKStateChangeMessage, FVKMsgId);
  TMessageManager.DefaultManager.Unsubscribe(TSizeChangedMessage, FSizeMsgId);
  inherited;
end;

procedure TVKStateHandler.DoSizeChanged(const Sender: TObject; const Msg: System.Messaging.TMessage);
var
  ASizeMsg: TSizeChangedMessage absolute Msg;
  R: TRect;
  AScene: IScene;
  AScale: Single;
begin
  if Sender = Screen.ActiveForm then
    begin
      if GetVKBounds(R, ASizeMsg.Value) then
        begin
          if Assigned(FLastControl) then
            begin
              if FLastControl = FindCommonLayout(Screen.ActiveForm) then
                FLastControl.SetBounds(0, 0, Screen.ActiveForm.Width, Screen.ActiveForm.height)
              else //
                TCustomScrollBox(FLastControl).Margins.bottom := 0;
            end;
          { if Supports(Sender, IScene, AScene) then
            begin
            AScale := AScene.GetSceneScale;
            R.Left := trunc(R.Left / AScale);
            R.Top := trunc(R.Top / AScale);
            R.Right := trunc(R.Right / AScale);
            R.Bottom := trunc(R.Bottom / AScale); }
          TMessageManager.DefaultManager.SendMessage(Sender, TVKStateChangeMessage.Create(true, R));
          // end;
        end
    end;
end;

procedure TVKStateHandler.DoVKVisibleChanged(const Sender: TObject; const Msg: System.Messaging.TMessage);
var
  AVKMsg: TVKStateChangeMessage absolute Msg;
  ACtrl: TControl;
  ACtrlBounds, AVKBounds, ATarget: TRectF;

  procedure MoveCtrls(AOldParent, ANewParent: TFmxObject);
  var
    I: Integer;
    AChild: TFmxObject;
  begin
    I := 0;
    while I < AOldParent.ChildrenCount do
      begin
        AChild := AOldParent.Children[I];
        if AChild <> ANewParent then
          begin
            if AChild.Parent = AOldParent then
              if not ControlIsBackground(AChild, AOldParent) then
                begin
                  AChild.Parent := ANewParent;
                  Continue;
                end;
          end;
        Inc(I);
      end;
  end;

  procedure AdjustByLayout(R: TRectF; ARoot: TFmxObject);
  var
    ALayout: TLayout;
  begin
    ALayout := FindCommonLayout(ARoot);
    if Assigned(ALayout) then
      begin
        FLastAlign := ALayout.Align;
        FLastBounds := ALayout.BoundsRect;
        FLastMargin := ALayout.Position.Point;
        ALayout.Align := TAlignLayout.None;
      end
    else
      begin
        ALayout := TLayout.Create(ARoot);
        ALayout.Parent := ARoot;
        ALayout.Name := sVKBHelperLayout + FLayoutTag.ToString;
        Inc(FLayoutTag);
        if ARoot is TCommonCustomForm then
          ALayout.SetBounds(0, R.bottom - ACtrlBounds.bottom, TCommonCustomForm(ARoot).Width, TCommonCustomForm(ARoot).height);
        MoveCtrls(ARoot, ALayout);
        FLastMargin.Y := 0;
        FLastMargin.X := 0;
        FLastAlign := TAlignLayout.Client;
        FLastBounds := ALayout.BoundsRect;
      end;
    if ARoot is TCommonCustomForm then
      ALayout.SetBounds(0, R.bottom - ACtrlBounds.bottom, TCommonCustomForm(ARoot).Width, TCommonCustomForm(ARoot).height);
    if FLastControl <> ALayout then
      begin
        if Assigned(FLastControl) then
          FLastControl.RemoveFreeNotification(Self);
        FLastControl := ALayout;
        FLastControl.FreeNotification(Self);
        EnableVKCheck(true);
      end;
  end;

  procedure ScrollInToRect(R: TRectF);
  var
    AParent, ALastParent: TFmxObject;
    AParentBounds: TRectF;
    AScrollBox: TCustomScrollBox;
    AOffset: Single;
  begin
    AParent := ACtrl.Parent;
    AScrollBox := nil;
    ALastParent := AParent;
    while Assigned(AParent) do
      begin
        if AParent is TCustomScrollBox then
          begin
            AScrollBox := AParent as TCustomScrollBox;
            AParentBounds := AScrollBox.AbsoluteRect;
            if AParentBounds.Contains(R) then
              begin
                AOffset := ACtrlBounds.top - R.top;
                if (AParentBounds.bottom > AVKBounds.top) or (AParentBounds.bottom < AParentBounds.height) then
                  begin
                    if (FLastControl <> AScrollBox) then
                      begin
                        if Assigned(FLastControl) then
                          FLastControl.RemoveFreeNotification(Self);
                        FLastMargin.Y := AScrollBox.Margins.bottom;
                        FLastMargin.X := AScrollBox.Margins.left;
                        FLastControl := AScrollBox;
                        FLastControl.FreeNotification(Self);
                        EnableVKCheck(true);
                      end;
                    AScrollBox.Margins.bottom := AParentBounds.bottom - AVKBounds.top;
                  end;
                AScrollBox.ViewportPosition := TPointF.Create(AScrollBox.ViewportPosition.X, AScrollBox.ViewportPosition.Y + AOffset);
                Break;
              end;
          end;
        ALastParent := AParent;
        AParent := AParent.Parent;
      end;
    if not Assigned(AScrollBox) then
      AdjustByLayout(R, ALastParent);
  end;

  function GetParentForm(Ctrl: TControl): TCommonCustomForm;
  var
    tmpParent: TFmxObject;
  begin
    Result := nil;
    tmpParent := Ctrl.Parent;
    while Assigned(tmpParent) do
      begin
        if tmpParent is TCommonCustomForm then
          Result := TCommonCustomForm(tmpParent);
        tmpParent := tmpParent.Parent;
      end;
  end;

var
  tmpParentForm: TCommonCustomForm;
  bCommonLayoutOffsetted: Boolean;
  ALayout: TLayout;
begin
  if AVKMsg.KeyboardVisible then
    begin
      if Screen.FocusControl <> nil then
        begin
          ACtrl := Screen.FocusControl.GetObject as TControl;

          tmpParentForm := GetParentForm(ACtrl);
          ALayout := FindCommonLayout(tmpParentForm);

          ACtrlBounds := ACtrl.AbsoluteRect;
          if Assigned(ALayout) then
            if (ALayout.Align = TAlignLayout.None) and (ALayout.Position.Y <> 0) then
              ACtrlBounds.Offset(0, -ALayout.Position.Y);

          AVKBounds := TRectF.Create(AVKMsg.KeyboardBounds);
          if Assigned(tmpParentForm) then
            begin
              AVKBounds.TopLeft := tmpParentForm.ScreenToClient(AVKBounds.TopLeft);
              AVKBounds.BottomRight := tmpParentForm.ScreenToClient(AVKBounds.BottomRight);
              // AVKBounds.Offset(0, tmpParentForm.top + tmpParentForm.height - AVKBounds.bottom);
            end;

          FglVKB := AVKBounds;

          bCommonLayoutOffsetted := False;
          if Assigned(ALayout) then
            if (ALayout.Align = TAlignLayout.None) then
              bCommonLayoutOffsetted := true;
          if bCommonLayoutOffsetted or (ACtrlBounds.bottom > AVKBounds.top) or (ACtrlBounds.top < 0) then
            begin
              ATarget := ACtrlBounds;
              ATarget.top := AVKBounds.top - ACtrlBounds.height;
              ATarget.bottom := AVKBounds.top;
              ScrollInToRect(ATarget);
            end
        end
    end
  else
    begin
      FglVKB := TRectF.Empty;
      if Assigned(FLastControl) then
        begin
          if FLastControl is TCustomScrollBox then
            begin
              FLastControl.Margins.bottom := FLastMargin.Y;
              FLastControl.Margins.left := FLastMargin.X;
            end
          else
            begin
              if StartsStr(sVKBHelperLayout, FLastControl.Name) then
                FLastControl.Align := TAlignLayout.Client
              else
                if FLastAlign = TAlignLayout.None then
                  begin
                    FLastControl.Position.Point := FLastMargin;
                  end
                else
                  begin
                    FLastControl.BoundsRect := FLastBounds;
                    FLastControl.Align := FLastAlign;
                  end;
            end;
          FLastControl := nil;
          EnableVKCheck(False);
        end;
    end;
end;

procedure TVKStateHandler.DoVKVisibleCheck(ASender: TObject);
begin
  if not IsVKVisible then
    begin
      EnableVKCheck(False);
      if Assigned(Screen.FocusControl) then
        TMessageManager.DefaultManager.SendMessage(Screen.FocusControl.GetObject, TVKStateChangeMessage.Create(False, TRect.Create(0, 0, 0, 0)));
    end;
end;

procedure TVKStateHandler.EnableVKCheck(AEnabled: Boolean);
begin
  // FVKVisibleTimer.Enabled := AEnabled;
end;

procedure TVKStateHandler.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
    begin
      if FLastControl = AComponent then
        FLastControl := nil;
    end;
  inherited;
end;

initialization

FglVKB := TRectF.Empty;
VKHandler := TVKStateHandler.Create(nil);

finalization

FreeAndNil(VKHandler);

end.
