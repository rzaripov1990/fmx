program GPstyle;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FormGP},
  FMX.ListView.Horz in 'component\FMX.ListView.Horz.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormGP, FormGP);
  Application.Run;
end.
