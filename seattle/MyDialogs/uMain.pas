unit uMain;

{
  author: krapotkin

  * modified by ZuBy, 2016

  * Переписан код, удалены лишние компоненты
  * Подогнал под стандартные диалоги платформ
  * Смена позиции кнопок "Oк", "Отмена" в зависимости от платформы
  * Профиксил MaxLength для TEdit (при установке ограничении ввода символов для мобильных платформ)
  * Возможность использования анонимных методов (TThreadProcedure)
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit;

type
  TFormMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Timer1: TTimer;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
  private
    { Private declarations }
    procedure DialogEvent(Sender: TObject);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses UMyInputQuery, UMyMsgDlg, UMyToast;

procedure TFormMain.Button1Click(Sender: TObject);
var
  mY: TMyInputQuery;
begin
  mY := TMyInputQuery.Create(['Введите логин:', 'Введите пароль:', 'aaaa', 'bbbb'], ['', '', '', ''], self,
    DialogEvent);
  mY.Edits[1].Password := true;
  mY.ShowMe;
end;

procedure TFormMain.Button2Click(Sender: TObject);
var
  mY: TMyInputQuery;
begin
  mY := TMyInputQuery.Create(['Введите текст'], [''], self,
    procedure
    begin
      ShowMessage('TThreadProcedure ' + mY.Values[0]);
    end);
  mY.Edits[0].Password := true;
  mY.ShowMe;
end;

procedure TFormMain.Button3Click(Sender: TObject);
var
  mY: TMyMsgDlg;
begin
  mY := TMyMsgDlg.Create('Вывести сообщение по закрытию окна?', self,
    procedure
    begin
      ShowMessage('Сообщение по закрытию окна');
    end, false);
  mY.ShowMe;
end;

procedure TFormMain.Button4Click(Sender: TObject);
var
  mY: TMyMsgDlg;
begin
  mY := TMyMsgDlg.Create('Вывести сообщение по закрытию окна?', self,
    procedure
    begin
      ShowMessage('Сообщение по закрытию окна');
    end, true);
  mY.ShowMe;
end;

procedure TFormMain.DialogEvent(Sender: TObject);
begin
  ShowMessage('TNotifyEvent' + (Sender as TMyInputQuery).Values[0] + ', ' + (Sender as TMyInputQuery).Values[1]);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  CheckBox1.Text := 'GlobalUseDirect2D';
  CheckBox1.IsChecked := GlobalUseDirect2D;

  CheckBox2.Text := 'GlobalUseDX';
  CheckBox2.IsChecked := GlobalUseDX;

  CheckBox3.Text := 'GlobalUseDXSoftware';
  CheckBox3.IsChecked := GlobalUseDXSoftware;

  CheckBox4.Text := 'GlobalUseGPUCanvas';
  CheckBox4.IsChecked := GlobalUseGPUCanvas;
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
var
  mT: TMyToast;
begin
  mT := TMyToast.Create('Вывести сообщение по закрытию окна?', self, 2000 + random(3000), 20 + random(100),
    TAlphaColorF.Create(random(255) / 255, random(255) / 255, random(255) / 255, 1).ToAlphaColor);
  mT.ShowMe;
end;

procedure TFormMain.Button5Click(Sender: TObject);
begin
//  Timer1.Enabled := not Timer1.Enabled;
   Timer1Timer(nil);
end;

procedure TFormMain.CheckBox1Change(Sender: TObject);
begin
  GlobalUseDirect2D := CheckBox1.IsChecked;
end;

procedure TFormMain.CheckBox2Change(Sender: TObject);
begin
  GlobalUseDX := CheckBox2.IsChecked;
end;

procedure TFormMain.CheckBox3Change(Sender: TObject);
begin
  GlobalUseDXSoftware := CheckBox3.IsChecked;
end;

procedure TFormMain.CheckBox4Change(Sender: TObject);
begin
  GlobalUseGPUCanvas := CheckBox4.IsChecked;
end;

end.
