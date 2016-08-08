program prg_Bass;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {FormMain},
  BASSFunctions in 'BASSFunctions.pas',
  ID3v1Library in 'ID3v1Library.pas',
  ID3v2Library in 'ID3v2Library.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
