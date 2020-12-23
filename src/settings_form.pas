unit settings_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  Buttons, StdCtrls, Spin, JsonTools, LCLType, Grids, SpinEx, ChronoUtility;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    DateFormatCombo: TComboBox;
    DateFormatEdit: TEdit;
    Label11: TLabel;
    ScheduleActiveLabel: TLabel;
    DatasetActiveLabel: TLabel;
    CenterLabel: TLabel;
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
    DatasetGrid: TStringGrid;
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
    procedure DailyCheckboxChange(Sender: TObject);
    procedure DatasetGridCheckboxToggled(sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure DateFormatComboChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HourlyCheckboxChange(Sender: TObject);
    procedure MiscButtonClick(Sender: TObject);
    procedure MiscPageBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure MonthlyCheckboxChange(Sender: TObject);
    procedure DatasetButtonClick(Sender: TObject);
    procedure ScheduleButtonClick(Sender: TObject);
    procedure WeeklyCheckboxChange(Sender: TObject);

  private
    procedure UpdateShieldIcon();

  public
    class var
      ConfigDir: ansistring;
      ConfigFile: ansistring;
  end;

  TDateFormat = class(TObject)
    public
      Format: ansistring;
      constructor Create(s: ansistring);
  end;

var
  SettingsForm: TSettingsForm;

implementation

{$R *.lfm}

constructor TDateFormat.Create(s: ansistring);
begin
  inherited Create();
  Format := s;
end;

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
  for i := 0 to DatasetGrid.RowCount - 1 do begin
    if DatasetGrid.Rows[i][0] = '1' then DatasetState := True;
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

procedure TSettingsForm.MiscButtonClick(Sender: TObject);
begin
  SettingsNotebook.PageIndex := 1;
end;

procedure TSettingsForm.MiscPageBeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
var
  Date, ConfigDateFormat, Error: ansistring;
  Idx: Integer;
  obj: TObject;
  Config: TJsonNode;
  DFormat: TDateFormat;
begin
  for Idx := 1 to DateFormatCombo.Items.Count - 1 do begin
    // Yes, I know this leaks memory the last time you open the page.  No, I don't care.  It will
    // be cleaned up at program termination.  Yes, I know that's bad pratice.  No, I don't care.
    obj := DateFormatCombo.Items.Objects[Idx];
    if obj <> nil then obj.Destroy();
  end;

  DateFormatCombo.Clear();

  ConfigDateFormat := '';
  DateFormatCombo.AddItem('Custom', nil);

  ConfigDateFormat := 'yyyy-m-d h:nn:ss';
  Date := FormatDateTime(ConfigDateFormat, Now);
  DateFormatCombo.Additem(Date, TDateFormat.Create(ConfigDateFormat));

  ConfigDateFormat := 'yyyy-m-d h:nn:ss AM/PM';
  Date := FormatDateTime(ConfigDateFormat, Now);
  DateFormatCombo.Additem(Date, TDateFormat.Create(ConfigDateFormat));

  ConfigDateFormat := 'd mmm yyyy hh:nn:ss AM/PM';
  Date := FormatDateTime(ConfigDateFormat, Now);
  DateFormatCombo.Additem(Date, TDateFormat.Create(ConfigDateFormat));

  ConfigDateFormat := 'yyyy mmm d, hh:nn:ss AM/PM';
  Date := FormatDateTime(ConfigDateFormat, Now);
  DateFormatCombo.Additem(Date, TDateFormat.Create(ConfigDateFormat));

  ConfigDateFormat := 'ddd d mmm yyyy hh:nn:ss AM/PM';
  Date := FormatDateTime(ConfigDateFormat, Now);
  DateFormatCombo.Additem(Date, TDateFormat.Create(ConfigDateFormat));

  if (GetConfigLocation(ConfigDir, ConfigFile, Error)) and
     (FileExists(ConfigDir + ConfigFile)) then begin
    Config := TJsonNode.Create();
    Config.LoadFromFile(ConfigDir + ConfigFile);

    if Config.Find('date_time_format') <> nil then begin
      ConfigDateFormat := Config.Find('date_time_format').Value.DeQuotedString('"');
      for Idx := 0 to DateFormatCombo.Items.Count -1 do begin
        DFormat := (DateFormatCombo.Items.Objects[Idx] as TDateFormat);
        if (DFormat <> nil) and (DFormat.Format = ConfigDateFormat) then begin
          DateFormatCombo.ItemIndex := Idx;
          DateFormatCombo.OnChange(ASender);
          break;
        end;
      end;
      if DateFormatEdit.Text = '' then begin
        // There is a date format, but it doesn't match any of the options.  Must be custom
        DateFormatEdit.Text := ConfigDateFormat;
        DateFormatCombo.ItemIndex := 0;
        DateFormatCombo.OnChange(ASender);
        DateFormatEdit.Enabled := true;
      end;
    end;
  end;

  if DateFormatEdit.Text = '' then begin
    DateFormatCombo.ItemIndex := 1;
    DateFormatCombo.OnChange(ASender);
  end;
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
  i: Integer;
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
      Node.Find('enabled').AsBoolean := MonthlyCheckbox.Checked;
      Node.Find('keep').Value := MonthlyEdit.Value.ToString();
  end;

  Node := Config.Find('weekly');
  if (Node = nil) then begin
    Node := Config.Add('weekly');
    Node.Add('enabled', WeeklyCheckbox.Checked);
    Node.Add('keep', WeeklyEdit.Value);
  end
  else begin
      Node.Find('enabled').AsBoolean := WeeklyCheckbox.Checked;
      Node.Find('keep').Value := WeeklyEdit.Value.ToString();
  end;

  Node := Config.Find('daily');
  if (Node = nil) then begin
    Node := Config.Add('daily');
    Node.Add('enabled', DailyCheckbox.Checked);
    Node.Add('keep', DailyEdit.Value);
  end
  else begin
      Node.Find('enabled').AsBoolean := DailyCheckbox.Checked;
      Node.Find('keep').Value := DailyEdit.Value.ToString();
  end;

  Node := Config.Find('hourly');
  if (Node = nil) then begin
    Node := Config.Add('hourly');
    Node.Add('enabled', HourlyCheckbox.Checked);
    Node.Add('keep', HourlyEdit.Value);
  end
  else begin
      Node.Find('enabled').AsBoolean := HourlyCheckbox.Checked;
      Node.Find('keep').Value := HourlyEdit.Value.ToString();
  end;

  Node := Config.Find('boot');
  if (Node = nil) then begin
    Node := Config.Add('boot');
    Node.Add('enabled', BootCheckbox.Checked);
    Node.Add('keep', BootEdit.Value);
  end
  else begin
      Node.Find('enabled').AsBoolean := BootCheckbox.Checked;
      Node.Find('keep').Value := BootEdit.Value.ToString();
  end;

  Node := Config.Find('datasets');
  if (Node = nil) then Node := Config.Add('datasets');

  for i := 1 to DatasetGrid.RowCount - 1 do begin
    DatasetName := DatasetGrid.Rows[i][1];
    if DatasetGrid.Rows[i][0] = '1' then begin
      if not Node.Find(DatasetName) then Node.Add(DatasetName);
    end
    else begin
      Node.Delete(DatasetName); // Ok to delete node that doesn't exist
    end;
  end;

  Config.Add('date_time_format', DateFormatEdit.Text);
  Config.SaveToFile(ConfigDir + ConfigFile);
  Config.Free();
end;

procedure TSettingsForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  TestDT: string;
begin
  try
    CanClose := true;
    TestDT := FormatDateTime(DateFormatEdit.Text, Now);
  except
    on E: Exception do begin
      Application.MessageBox(PChar('DateTime format is not valid.'), 'Chronology - Error', MB_ICONERROR + MB_OK);
      CanClose := false;
    end;
  end;
end;

procedure TSettingsForm.DailyCheckboxChange(Sender: TObject);
begin
    UpdateShieldIcon();
end;

procedure TSettingsForm.DatasetGridCheckboxToggled(sender: TObject; aCol,
  aRow: Integer; aState: TCheckboxState);
begin
  UpdateShieldIcon();
end;

procedure TSettingsForm.DateFormatComboChange(Sender: TObject);
var
  Format: TDateFormat;
begin
  Format := TDateFormat(DateFormatCombo.Items.Objects[DateFormatCombo.ItemIndex]);
  if Format = nil then DateFormatEdit.Enabled := true
  else begin
    DateFormatEdit.Enabled := false;
    DateFormatEdit.Text := Format.Format;
  end;
end;

procedure TSettingsForm.BootCheckboxChange(Sender: TObject);
begin
    UpdateShieldIcon();
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
var
  Error: ansistring;
begin
  if (not GetConfigLocation(ConfigDir, ConfigFile, Error)) then begin
    Application.MessageBox(PChar(Error), 'Chronology - Error', MB_ICONERROR + MB_OK);
    Close();
  end;
end;

procedure TSettingsForm.FormShow(Sender: TObject);
var
  Config, Node: TJsonNode;
  i, j: Integer;
  Error: ansistring;

begin
  DatasetGrid.FocusColor := clWindow;

  if not ListDatasets(DatasetGrid, Error) then begin
     Application.MessageBox(PChar('Error listing datasets.'), 'Chronology - Error', MB_ICONERROR + MB_OK);
     Exit();
  end;

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
      for j := 1 to DatasetGrid.RowCount - 1 do begin;
        if DatasetGrid.Rows[j][1] = Node.Child(i).Name then begin
          DatasetGrid.Rows[j][0] := '1';
          break;
        end;
      end;
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

