unit settings_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  Buttons, StdCtrls, Spin, JsonTools, LCLType, SpinEx;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    ActiveLabel: TLabel;
    CenterLabel: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LatestSnapsot: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    SettingsOkButton: TBitBtn;
    MonthlyCheckbox: TCheckBox;
    WeeklyCheckbox: TCheckBox;
    DailyCheckbox: TCheckBox;
    HourlyCheckbox: TCheckBox;
    BootCheckbox: TCheckBox;
    StopEmailCheckox: TCheckBox;
    MonthlyEdit: TSpinEdit;
    WeeklyEdit: TSpinEdit;
    DailyEdit: TSpinEdit;
    HourlyEdit: TSpinEdit;
    BootEdit: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    SettingsNotebook: TNotebook;
    SchedulePage: TPage;
    MiscPage: TPage;
    ScheduleButton: TButton;
    MiscButton: TButton;
    Panel1: TPanel;
    ShieldImage: TImage;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MiscButtonClick(Sender: TObject);
    procedure ScheduleButtonClick(Sender: TObject);
    procedure SettingsOkButtonClick(Sender: TObject);
  private

  public
    class var
      ConfigDir: ansistring;
      ConfigFile: ansistring;
  end;

var
  SettingsForm: TSettingsForm;

implementation

{$R *.lfm}

{ TSettingsForm }

procedure TSettingsForm.ScheduleButtonClick(Sender: TObject);
begin
  SettingsNotebook.PageIndex := 0;

end;

procedure TSettingsForm.SettingsOkButtonClick(Sender: TObject);
begin

end;

procedure TSettingsForm.MiscButtonClick(Sender: TObject);
begin
  SettingsNotebook.PageIndex := 1;
end;

procedure TSettingsForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Config, Node: TJsonNode;

begin
  if (not DirectoryExists(ConfigDir)) then begin
    if (not ForceDirectories(ConfigDir)) then begin
        Application.MessageBox(PChar('Could not create directory: ' + ConfigDir),
              'Chronology - Error', MB_ICONERROR + MB_OK);
      Exit();
    end;
  end;

  Config := TJsonNode.Create();

  if (FileExists(ConfigDir + ConfigFile)) then
     Config.LoadFromFile(ConfigDir +  ConfigFile);


  if (Config.Find('stop_cron_email') <> nil) then
     Config.Delete('stop_cron_email');
  Config.Add('stop_cron_email', StopEmailCheckox.Checked);

  if (MonthlyCheckbox.Checked = true) then begin
    Node := Config.Add('monthly');
    Node.Add('enabled', true);
    Node.Add('keep', MonthlyEdit.Value);
  end
  else if (Config.Find('monthly') <> nil) then
     Config.Delete('monthly');

  if (WeeklyCheckbox.Checked = true) then begin
    Node := Config.Add('weekly');
    Node.Add('enabled', true);
    Node.Add('keep', WeeklyEdit.Value);
  end
  else if (Config.Find('weekly') <> nil) then
     Config.Delete('weekly');

  if (DailyCheckbox.Checked = true) then begin
    Node := Config.Add('daily');
    Node.Add('enabled', true);
    Node.Add('keep', DailyEdit.Value);
  end
  else if (Config.Find('daily') <> nil) then
     Config.Delete('daily');

  if (HourlyCheckbox.Checked = true) then begin
    Node := Config.Add('hourly');
    Node.Add('enabled', true);
    Node.Add('keep', HourlyEdit.Value);
  end
  else if (Config.Find('hourly') <> nil) then
     Config.Delete('hourly');

  if (BootCheckbox.Checked = true) then begin
    Node := Config.Add('boot');
    Node.Add('enabled', true);
    Node.Add('keep', BootEdit.Value);
  end
  else if (Config.Find('boot') <> nil) then
     Config.Delete('boot');

  Config.SaveToFile(ConfigDir + ConfigFile);
  Config.Free();
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
var
  UID, UserName: ansistring;
  NumRead: Word;
  Buf: ansistring;
  Passwd: TextFile;
  A: TStringArray;

begin
  UserName := '';
  UID := GetEnvironmentVariable('PKEXEC_UID');

  if (UID = '') then
     UserName := GetEnvironmentVariable('USER')
  else begin
    System.Assign(Passwd, '/etc/passwd');
    Reset(Passwd);
    while not EoF(Passwd) do begin
      Readln(Passwd, Buf);
        A := Buf.Split(':');
        if (A[2] = UID) then begin
          UserName := A[0];
          Break;
        end;
    end;
    CloseFile(Passwd);
  end;

  if (UserName = '') then begin
     Application.MessageBox('Unable to find user name.  Aborting',
                                    'Chronology - Error', MB_ICONERROR + MB_OK);
     Close();
  end;

  ConfigDir := '/home/' + UserName + '/.config/chronology/';
  ConfigFile := 'settings.json';
end;

end.

