unit UBaseFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type
  TMyBaseFrame = class(TFrame)
  private
    { Private declarations }
  public
    procedure Init; virtual;
  end;

implementation

{$R *.fmx}
{ TMyBaseFrame }

procedure TMyBaseFrame.Init;
begin

end;

end.
