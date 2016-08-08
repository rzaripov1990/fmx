program LVBitmap;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {Form16},
  FMX.Bitmap.Helpers in 'FMX.Bitmap.Helpers.pas',
  FMX.ListView in 'FMX.ListView.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm16, Form16);
  Application.Run;
end.
