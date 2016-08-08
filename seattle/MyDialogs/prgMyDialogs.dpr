program prgMyDialogs;

uses
  System.StartUpCopy,
  FMX.Forms,
  UBaseDialog in 'UBaseDialog.pas' {fBaseDialog} ,
  uGlobal in 'uGlobal.pas',
  uMain in 'uMain.pas' {FormMain} ,
  UMyInputQuery in 'UMyInputQuery.pas' {MyInputQuery} ,
  UMyMsgDlg in 'UMyMsgDlg.pas' {MyMsgDlg} ,
  vkbdhelper in 'vkbdhelper.pas',
  UMyToast in 'UMyToast.pas' {MyToast};

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := true;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TfBaseDialog, fBaseDialog);
  Application.Run;

end.
