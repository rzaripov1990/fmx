unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView, FMX.Ani;

type
  TFormMain = class(TForm)
    tbHeader: TRectangle;
    lvFiles: TListView;
    sbRefresh: TSpeedButton;
    tbControls: TRectangle;
    sbPlay: TSpeedButton;
    sbPause: TSpeedButton;
    tbPosition: TTrackBar;
    tmPosition: TTimer;
    lbTitle: TLabel;
    procedure sbRefreshClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure lvFilesItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure sbPlayTap(Sender: TObject; const Point: TPointF);
    procedure sbPauseTap(Sender: TObject; const Point: TPointF);
    procedure tmPositionTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

  aPath: string = '';
  mCh: DWORD = 0;

implementation

{$R *.fmx}

uses
  System.IOUtils, BASSFunctions, ID3v1Library, ID3v2Library;

function GetMusicPath: string;
begin
  Result := TPath.GetSharedMusicPath;
  if not TDirectory.Exists(Result) then
    Result := '';
end;

function readTagFromFile(aFile: string): string;
var
  aTitle, aArtist: string;

  function IFV: string;
  begin
    if (not aTitle.IsEmpty) and (not aArtist.IsEmpty) then
      Result := aArtist + ' - ' + aTitle
    else if (not aTitle.IsEmpty) and (aArtist.IsEmpty) then
      Result := aTitle
    else if (aTitle.IsEmpty) and (not aArtist.IsEmpty) then
      Result := aArtist
    else
      Result := TPath.GetFileName(aFile);
  end;

var
  TryGetValueFromV1: Boolean;

begin
  Result := TPath.GetFileName(aFile);

  TryGetValueFromV1 := true;

  with TID3v2Tag.Create do
  begin
    LoadFromFile(aFile);
    if Loaded then
    begin
      TryGetValueFromV1 := false;
      aTitle := GetUnicodeText('TIT2');
      aArtist := GetUnicodeText('TPE1');
      Result := IFV;
    end;
    Free;
  end;

  if TryGetValueFromV1 then
  begin
    with TID3v1Tag.Create do
    begin
      LoadFromFile(aFile);
      if Loaded then
      begin
        aTitle := Title;
        aArtist := Artist;
        Result := IFV;
      end;
      Free;
    end;
  end;
end;

procedure LVadd(const aFile: string; aLV: TListView);
begin
  with aLV.Items.Add do
  begin
    Text := readTagFromFile(aFile);
    Data['file'] := aFile;
  end;
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
{$IFDEF ANDROID}
  if BassLoaded then
    exit;

  if LoadBASS then
  begin
    if not BASS_Init(-1, 44100, 0, nil, nil) then
      ShowMessage('BASS not Init!');
  end
  else
    ShowMessage('BASS library not loaded!');
{$ENDIF}
end;

procedure TFormMain.lvFilesItemClick(const Sender: TObject; const AItem: TListViewItem);
begin
  if BassLoaded then
  begin
    if mCh > 0 then
    begin
      BASS_StreamFree(mCh);
      tbPosition.Value := 0;
      tbPosition.Max := 0;
      tmPosition.Enabled := false;
    end;

    mCh := BASS_StreamCreateFile(false, pointer(AItem.Data['file'].AsString), 0, 0, BASS_STREAM_PRESCAN or
      BASS_STREAM_STATUS or BASS_STREAM_AUTOFREE or BASS_UNICODE);

    if mCh > 0 then
    begin
      tbPosition.Max := BASS_ChannelBytes2Seconds(mCh, BASS_ChannelGetLength(mCh, BASS_POS_BYTE));
      tbPosition.Value := BASS_ChannelBytes2Seconds(mCh, BASS_ChannelGetPosition(mCh, BASS_POS_BYTE));

      tmPosition.Enabled := true;
      BASS_ChannelPlay(mCh, false);
    end;
  end;
end;

procedure TFormMain.sbPauseTap(Sender: TObject; const Point: TPointF);
begin
  if BassLoaded and (mCh > 0) then
    BASS_ChannelPause(mCh);
end;

procedure TFormMain.sbPlayTap(Sender: TObject; const Point: TPointF);
begin
  if BassLoaded and (mCh > 0) then
    BASS_ChannelPlay(mCh, false);
end;

procedure TFormMain.sbRefreshClick(Sender: TObject);
var
  aFiles: TStringDynArray;
  I: Integer;
begin
  aPath := GetMusicPath;
  if not aPath.IsEmpty then
  begin
    aFiles := TDirectory.GetFiles(aPath);
    if Length(aFiles) > 0 then
      for I := Low(aFiles) to High(aFiles) do
      begin
        LVadd(aFiles[I], lvFiles);
      end;
  end
  else
    ShowMessage('Файлы не найдены в директории ' + #13#10 + aPath);
end;

procedure TFormMain.tmPositionTimer(Sender: TObject);
begin
  if BassLoaded and (mCh > 0) then
    tbPosition.Value := BASS_ChannelBytes2Seconds(mCh, BASS_ChannelGetPosition(mCh, BASS_POS_BYTE));
end;

end.
