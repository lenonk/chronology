unit settings_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  Buttons, StdCtrls, Spin, JsonTools, LCLType, SpinEx, ChronoUtility;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    ScheduleActiveLabel: TLabel;
    DatasetActiveLabel: TLabel;
    CenterLabel: TLabel;
    DatasetsCheckGroup: TCheckGroup;
    DatasetIntervalLabel: TLabel;
    Label10: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ScheduleIntervalLabel: TLabel;
    Panel4: TPanel;
    Panel5: TPanel;
    DatasetPage: TPage;
    DatasetButton: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    SettingsOkButton: TBitBtn;
    MonthlyCheckbox: TCheckBox;
    DatasetShieldImage: TImage;
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
    ScheduleShieldImage: TImage;
    procedure BootCheckboxChange(Sender: TObject);
    procedure DatasetsCheckGroupItemClick(Sender: TObject; Index: integer);
    procedure DailyCheckboxChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HourlyCheckboxChange(Sender: TObject);
    procedure MiscButtonClick(Sender: TObject);
    procedure MonthlyCheckboxChange(Sender: TObject);
    procedure DatasetButtonClick(Sender: TObject);
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
  ScheduleState, DatasetState: Boolean;
  i: Integer;
begin

  ScheduleState := True;
  if ((not MonthlyCheckbox.Checked)
      and (not WeeklyCheckbox.Checked)
      and (not DailyCheckbox.Checked)
      and (not HourlyCheckbox.Checked)
      and (not BootCheckbox.Checked)) then ScheduleState := false;

  DatasetState := False;
  for i := 0 to DatasetsCheckgroup.Items.Count - 1 do begin
    if (DatasetsCheckgroup.Checked[i]) then DatasetState := True;
  end;

  if (DatasetState = False) then begin
    ScheduleShieldImage.Picture.LoadFromResourceName(HInstance, 'SHIELD-WARNING-ICON');
    ScheduleActiveLabel.Caption := 'No datasets are selected';
    ScheduleIntervalLabel.Caption := 'Select the datasets to snapshot';

    DatasetShieldImage.Picture.LoadFromResourceName(HInstance, 'SHIELD-WARNING-ICON');
    DatasetActiveLabel.Caption := 'No datasets are selected';
    DatasetIntervalLabel.Caption := 'Select the datasets to snapshot';

  end
  else if (ScheduleState = True) then begin
    ScheduleShieldImage.Picture.LoadFromResourceName(HInstance, 'SHIELD-OK-ICON');
    ScheduleActiveLabel.Caption := 'Scheduled snapshots are enabled';
    ScheduleIntervalLabel.Caption := 'Snapshots will be created at selected ' +
                             'intervals if disk space allows';

    DatasetShieldImage.Picture.LoadFromResourceName(HInstance, 'SHIELD-OK-ICON');
    DatasetActiveLabel.Caption := 'Scheduled snapshots are enabled';
    DatasetIntervalLabel.Caption := 'Snapshots will be created at selected ' +
                             'intervals if disk space allows';
  end
  else begin
    ScheduleShieldImage.Picture.LoadFromResourceName(HInstance, 'SHIELD-WARNING-ICON');
    ScheduleActiveLabel.Caption := 'Scheduled snapshots are disabled';
    ScheduleIntervalLabel.Caption := 'Select the intervals for creating snapshots';

    DatasetShieldImage.Picture.LoadFromResourceName(HInstance, 'SHIELD-WARNING-ICON');
    DatasetActiveLabel.Caption := 'Scheduled snapshots are disabled';
    DatasetIntervalLabel.Caption := 'Select the intervals for creating snapshots';
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

procedure TSettingsForm.DatasetButtonClick(Sender: TObject);
begin
  SettingsNotebook.PageIndex := 2;
end;

procedure TSettingsForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Config, Node: TJsonNode;
  i, DatasetCount: Integer;
  DatasetName: ansistring;
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

  Node := Config.Find('datasets');
  if (Node = nil) then Node := Config.Add('datasets');

  DatasetCount := 0;
  for i := 0 to DatasetsCheckGroup.Items.Count - 1 do begin
    DatasetName := DatasetsCheckGroup.Items[i];
    if (DatasetsCheckGroup.Checked[i]) then begin
      Inc(DatasetCount);
      Node.Add(DatasetName); // TJsonTools will handle pre-existing
    end
    else begin
      Node.Delete(DatasetName);
    end;
  end;

  Node.Add('count', DatasetCount);

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

procedure TSettingsForm.DatasetsCheckGroupItemClick(Sender: TObject; Index: integer);
begin
  UpdateShieldIcon();
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  if (not GetConfigLocation(ConfigDir, ConfigFile)) then begin
     Close();
  end;
end;

procedure TSettingsForm.FormShow(Sender: TObject);
var
  Config, Node: TJsonNode;
  i, idx: Integer;

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

  Node := Config.Find('datasets');
  if (Node <> nil) then begin
    for i := 0 to Node.Count - 1 do begin
      if (Node.Child(i).Name = 'count') then continue;
      idx := DatasetsCheckGroup.Items.IndexOf(Node.Child(i).Name);
      if (idx >= 0) then
        DatasetsCheckGroup.Checked[idx] := true;
    end;
  end;

  Config.Free();
  UpdateShieldIcon();
end;

procedure TSettingsForm.HourlyCheckboxChange(Sender: TObject);
begin
    UpdateShieldIcon();
end;

end.

