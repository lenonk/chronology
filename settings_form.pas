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
    IntervalLabel: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    SettingsOkButton: TBitBtn;
    MonthlyCheckbox: TCheckBox;
    WeeklyCheckbox: TCheckBox;
    DailyCheckbox: TCheckBox;
    HourlyCheckbox: TCheckBox;
    BootCheckbox: TCheckBox;
    StopEmailCheckbox: TCheckBox;
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
    procedure BootCheckboxChange(Sender: TObject);
    procedure DailyCheckboxChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HourlyCheckboxChange(Sender: TObject);
    procedure MiscButtonClick(Sender: TObject);
    procedure MonthlyCheckboxChange(Sender: TObject);
    procedure ScheduleButtonClick(Sender: TObject);
    procedure SettingsOkButtonClick(Sender: TObject);
    procedure WeeklyCheckboxChange(Sender: TObject);

  private
    procedure UpdateShieldIcon();

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

procedure TSettingsForm.UpdateShieldIcon();
var
  State: Boolean;
begin
  State := True;
  if ((not MonthlyCheckbox.Checked)
      and (not WeeklyCheckbox.Checked)
      and (not DailyCheckbox.Checked)
      and (not HourlyCheckbox.Checked)
      and (not BootCheckbox.Checked)) then State := false;

  if (State = true) then begin
    ShieldImage.Picture.LoadFromResourceName(HInstance, 'SHIELD-OK-ICON');
    ActiveLabel.Caption := 'Scheduled snapshots are enabled';
    IntervalLabel.Caption := 'Snapshots will be created at selected ' +
                             'intervals if disk space allows';
  end
  else begin
      ShieldImage.Picture.LoadFromResourceName(HInstance, 'SHIELD-WARNING-ICON');
      ActiveLabel.Caption := 'Scheduled snapshots are disabled';
      IntervalLabel.Caption := 'Select the intervals for creating snapshots';
  end;
end;

procedure TSettingsForm.WeeklyCheckboxChange(Sender: TObject);
begin
    UpdateShieldIcon();
end;

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

procedure TSettingsForm.MonthlyCheckboxChange(Sender: TObject);
begin
  UpdateShieldIcon();
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

  Config.Delete('stop_cron_email');
  Config.Add('stop_cron_email', StopEmailCheckbox.Checked);

  Node := Config.Find('monthly');
  if (Node = nil) then begin
    Node := Config.Add('monthly');
    Node.Add('enabled', MonthlyCheckbox.Checked);
    Node.Add('keep', MonthlyEdit.Value);
  end
  else begin
      Node.Find('enabled').Value := MonthlyCheckbox.Checked.ToString();
      Node.Find('keep').Value := MonthlyEdit.Value.ToString();
  end;

  Node := Config.Find('weekly');
  if (Node = nil) then begin
    Node := Config.Add('weekly');
    Node.Add('enabled', WeeklyCheckbox.Checked);
    Node.Add('keep', WeeklyEdit.Value);
  end
  else begin
      Node.Find('enabled').Value := WeeklyCheckbox.Checked.ToString();
      Node.Find('keep').Value := WeeklyEdit.Value.ToString();
  end;

  Node := Config.Find('daily');
  if (Node = nil) then begin
    Node := Config.Add('daily');
    Node.Add('enabled', DailyCheckbox.Checked);
    Node.Add('keep', DailyEdit.Value);
  end
  else begin
      Node.Find('enabled').Value := DailyCheckbox.Checked.ToString();
      Node.Find('keep').Value := DailyEdit.Value.ToString();
  end;

  Node := Config.Find('hourly');
  if (Node = nil) then begin
    Node := Config.Add('hourly');
    Node.Add('enabled', HourlyCheckbox.Checked);
    Node.Add('keep', HourlyEdit.Value);
  end
  else begin
      Node.Find('enabled').Value := HourlyCheckbox.Checked.ToString();
      Node.Find('keep').Value := HourlyEdit.Value.ToString();
  end;

  Node := Config.Find('boot');
  if (Node = nil) then begin
    Node := Config.Add('boot');
    Node.Add('enabled', BootCheckbox.Checked);
    Node.Add('keep', BootEdit.Value);
  end
  else begin
      Node.Find('enabled').Value := BootCheckbox.Checked.ToString();
      Node.Find('keep').Value := BootEdit.Value.ToString();
  end;

  Config.SaveToFile(ConfigDir + ConfigFile);
  Config.Free();
end;

procedure TSettingsForm.DailyCheckboxChange(Sender: TObject);
begin
    UpdateShieldIcon();
end;

procedure TSettingsForm.BootCheckboxChange(Sender: TObject);
begin
    UpdateShieldIcon();
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

procedure TSettingsForm.FormShow(Sender: TObject);
var
  Config, Node: TJsonNode;

begin
  if (not FileExists(ConfigDir + ConfigFile)) then
    Exit();

  Config := TJsonNode.Create();
  Config.LoadFromFile(ConfigDir +  ConfigFile);

  StopEmailCheckbox.Checked := true;
  Node := Config.Find('stop_cron_email');
  if (Node <> nil) then
    StopEmailCheckbox.Checked := Node.value.ToBoolean();

  MonthlyCheckbox.Checked := false;
  Node := Config.Find('monthly');
  if (Node <> nil) then begin
    MonthlyCheckbox.Checked := Node.Find('enabled').Value.ToBoolean();
    MonthlyEdit.Value := Node.Find('keep').Value.ToInteger();
  end;

  WeeklyCheckbox.Checked := false;
  Node := Config.Find('weekly');
  if (Node <> nil) then begin
    WeeklyCheckbox.Checked := Node.Find('enabled').Value.ToBoolean();
    WeeklyEdit.Value := Node.Find('keep').Value.ToInteger();
  end;

  DailyCheckbox.Checked := false;
  Node := Config.Find('daily');
  if (Node <> nil) then begin
    DailyCheckbox.Checked := Node.Find('enabled').Value.ToBoolean();
    DailyEdit.Value := Node.Find('keep').Value.ToInteger();
  end;

  HourlyCheckbox.Checked := false;
  Node := Config.Find('hourly');
  if (Node <> nil) then begin
    HourlyCheckbox.Checked := Node.Find('enabled').Value.ToBoolean();
    HourlyEdit.Value := Node.Find('keep').Value.ToInteger();
  end;

  BootCheckbox.Checked := false;
  Node := Config.Find('boot');
  if (Node <> nil) then begin
    BootCheckbox.Checked := Node.Find('enabled').Value.ToBoolean();
    BootEdit.Value := Node.Find('keep').Value.ToInteger();
  end;

  Config.Free();

  UpdateShieldIcon();
end;

procedure TSettingsForm.HourlyCheckboxChange(Sender: TObject);
begin
    UpdateShieldIcon();
end;

end.

