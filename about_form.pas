unit about_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LCLIntf;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Label4: TLabel;
    LicenseButton: TButton;
    CreditsButton: TButton;
    CloseButton: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label4Click(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.CloseButtonClick(Sender: TObject);
begin
  Close();
end;

procedure TAboutForm.FormShow(Sender: TObject);
begin
    Label4.Font.Color := clBlue;
    Label4.Invalidate();
    Label4.Update();
    Label4.Repaint();
end;

procedure TAboutForm.Label4Click(Sender: TObject);
begin
  OpenURL('https://github.com/lenonk/chronology');
end;

end.

