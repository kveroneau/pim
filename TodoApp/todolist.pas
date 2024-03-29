unit todolist;

{ Built in 3 hours! }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls, Grids,
  StdCtrls, ExtCtrls, newtask, appsettings, StrUtils;

type

  { TTodoForm }

  TTodoForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    HelpMenu: TMenuItem;
    AboutMenu: TMenuItem;
    CSVMenu: TMenuItem;
    ImportMenu: TMenuItem;
    ExportMenu: TMenuItem;
    Label5: TLabel;
    Label6: TLabel;
    CurSessPt: TLabel;
    ArchiveMenu: TMenuItem;
    TodoLog: TMemo;
    SettingsMenu: TMenuItem;
    MenuItem3: TMenuItem;
    PurgeMenu: TMenuItem;
    SaveAsFile: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    LogPage: TTabSheet;
    TaskCount: TLabel;
    PointsEarned: TLabel;
    PointsUsed: TLabel;
    PointsRemaining: TLabel;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    StatsPage: TTabSheet;
    TaskMenu: TMenuItem;
    AddTaskMenu: TMenuItem;
    NewFile: TMenuItem;
    OpenFile: TMenuItem;
    StaticGrid: TStringGrid;
    RewardsGrid: TStringGrid;
    TaskGrid: TStringGrid;
    Tabs: TPageControl;
    SaveFile: TMenuItem;
    ExitApp: TMenuItem;
    StatusBar: TStatusBar;
    StaticPage: TTabSheet;
    RewardsPage: TTabSheet;
    TasksPage: TTabSheet;
    Timer: TTimer;
    procedure AboutMenuClick(Sender: TObject);
    procedure AddTaskMenuClick(Sender: TObject);
    procedure ArchiveMenuClick(Sender: TObject);
    procedure ExitAppClick(Sender: TObject);
    procedure ExportMenuClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImportMenuClick(Sender: TObject);
    procedure NewFileClick(Sender: TObject);
    procedure OpenFileClick(Sender: TObject);
    procedure PurgeMenuClick(Sender: TObject);
    procedure RewardsGridButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure SaveAsFileClick(Sender: TObject);
    procedure SaveFileClick(Sender: TObject);
    procedure SettingsMenuClick(Sender: TObject);
    procedure StaticGridButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure TabsChange(Sender: TObject);
    procedure TaskGridClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FEarned, FUsed, FCompleted, FCurSession: Integer;
    FModified: Boolean;
    FFileName: string;
    SettingsForm: TSettingsForm;
    procedure AdjustUI;
    procedure AddTask(points, title: string);
    procedure AddStaticTask(points, title: string);
    procedure AddReward(points, title: string);
    procedure UpdatePoints;
    procedure ClearAllData;
    procedure LoadDefaultFile;
    procedure UpdateMenu(TabName: string);
    procedure AddToLog(const msg: string);
    procedure UpdateStatus(const msg: string);
  public
    procedure LoadFromFile(fname: string);
    procedure SaveToFile(fname: string);
    procedure ArchiveTask(points, title: string);
  end;

var
  TodoForm: TTodoForm;

implementation

type
  TSignature = Array[0..3] of Char;
  EInvalidFile = Class(Exception);

const
  {$IFDEF DEBUG}
  DEFAULT_FILE = '/debugtasks.dat';
  {$ELSE}
  DEFAULT_FILE = '/tasks.dat';
  {$ENDIF}
  TD_SIG: TSignature = ('K','T','D','*');
  TD_VER = 2;

type
  TTodoHeader = packed record
    sig: TSignature;
    ver: byte;
    tasks: byte;
    stasks: byte;
    rewards: byte;
    points: word;
    used: word;
  end;

  TTodoSettings = packed record
    list_name: string[20];
    priority: byte;
    last_tab: byte;
    auto_save: boolean;
  end;

  TTodoItem = packed record
    title: string[80];
    priority: byte;
    done: boolean;
  end;

  TStaticItem = packed record
    title: string[80];
    points: byte;
  end;

  TTodoReward = packed record
    title: string[80];
    cost: byte;
  end;

{$R *.lfm}

{ TTodoForm }

procedure TTodoForm.FormCreate(Sender: TObject);
begin
  SettingsForm:=TSettingsForm.Create(Self);
  FModified:=False;
  FEarned:=0;
  FUsed:=0;
  FCompleted:=0;
  UpdatePoints;
  FFileName:='';
  UpdateStatus('Ready');
end;

procedure TTodoForm.AddTaskMenuClick(Sender: TObject);
var
  twin: TNewTaskForm;
  r: Integer;
begin
  twin:=TNewTaskForm.Create(Nil);
  twin.TaskPoints.Text:=SettingsForm.DefaultPriority.Text;
  if Tabs.ActivePage = TasksPage then
    twin.TaskType.ItemIndex:=0
  else if Tabs.ActivePage = StaticPage then
    twin.TaskType.ItemIndex:=1
  else if Tabs.ActivePage = RewardsPage then
    twin.TaskType.ItemIndex:=2;
  r:=twin.ShowModal;
  if r = mrOK then
  begin
    case twin.TaskType.ItemIndex of
      0: AddTask(twin.TaskPoints.Text, twin.TaskTitle.Text);
      1: AddStaticTask(twin.TaskPoints.Text, twin.TaskTitle.Text);
      2: AddReward(twin.TaskPoints.Text, twin.TaskTitle.Text);
    end;
  end;
  twin.Free;
  if SettingsForm.AutoSave.Checked then
    if FFileName <> '' then
      SaveToFile(FFileName);
end;

procedure TTodoForm.ArchiveMenuClick(Sender: TObject);
var
  ArcFName, o: string;
  ArcForm: TTodoForm;
  i, count: integer;
  PRows: Array of integer;
begin
  if FFileName = 'tasks.dat' then
    ArcFName:='archive.dat'
  else
    ArcFName:=Copy2Symb(FFileName, '.')+'.arc';
  WriteLn(FFileName);
  UpdateStatus('Archiving completed items to '+ArcFName+'...');
  Application.ProcessMessages;
  ArcForm:=TTodoForm.Create(Nil);
  try
    if FileExists(ArcFName) then
      ArcForm.LoadFromFile(ArcFName);
    count:=0;
    for i:=0 to TaskGrid.RowCount-1 do
      if TaskGrid.Cells[2,i] = '1' then
      begin
        Inc(count);
        SetLength(PRows, count); { Probably not efficient, but I'm not trying to be here. }
        PRows[count-1]:=i;
        AddToLog('Archiving '+TaskGrid.Cells[1,i]+'...');
        ArcForm.ArchiveTask(TaskGrid.Cells[0,i], TaskGrid.Cells[1,i]);
      end;
    ArcForm.SaveToFile(ArcFName);
    UpdateStatus('Archived '+IntToStr(count)+' items to '+ArcFName);
    Application.ProcessMessages;
    ArcForm.Top:=Top+40;
    ArcForm.Left:=Left+40;
    ArcForm.ShowModal;
  finally
    ArcForm.Free;
  end;
  for i:=count-1 downto 0 do
    TaskGrid.DeleteRow(PRows[i]);
  SetLength(PRows, 0);
  FModified:=True;
  FCompleted:=0;
  UpdatePoints;
end;

procedure TTodoForm.AboutMenuClick(Sender: TObject);
begin
  ShowMessage('Kevin''s Todo List Application v0.5');
end;

procedure TTodoForm.ExitAppClick(Sender: TObject);
begin
  Close;
end;

procedure TTodoForm.ExportMenuClick(Sender: TObject);
begin
  {$IFDEF UNIX}
  SaveDialog.InitialDir:=GetEnvironmentVariable('HOME');
  {$ENDIF}
  SaveDialog.DefaultExt:='csv';
  SaveDialog.Filter:='CSV Files|*.csv';
  if SaveDialog.Execute then
  begin
    if Tabs.ActivePage = TasksPage then
      TaskGrid.SaveToCSVFile(SaveDialog.FileName)
    else if Tabs.ActivePage = StaticPage then
      StaticGrid.SaveToCSVFile(SaveDialog.FileName)
    else if Tabs.ActivePage = RewardsPage then
      RewardsGrid.SaveToCSVFile(SaveDialog.FileName)
    else
      UpdateStatus('Please switch to the tab you wish to export from.');
  end;
end;

procedure TTodoForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  {$IFDEF DEBUG}
  Exit;
  {$ENDIF}
  if FModified and SettingsForm.AutoSave.Checked then
    if FFileName <> '' then
      SaveToFile(FFileName)
    else
      CloseAction:=caNone;
end;

procedure TTodoForm.FormResize(Sender: TObject);
begin
  AdjustUI;
end;

procedure TTodoForm.FormShow(Sender: TObject);
begin
  if fsModal in FormState then
  begin
    UpdatePoints;
    Exit;
  end;
  if ParamCount = 1 then
    Try
      LoadFromFile(ParamStr(1))
    Except
      On EInvalidFile do UpdateStatus('Error loading '+ParamStr(1));
    end
  else
    LoadDefaultFile;
end;

procedure TTodoForm.ImportMenuClick(Sender: TObject);
begin
  {$IFDEF UNIX}
  OpenDialog.InitialDir:=GetEnvironmentVariable('HOME');
  {$ENDIF}
  OpenDialog.DefaultExt:='csv';
  OpenDialog.Filter:='CSV Files|*.csv';
  if OpenDialog.Execute then
  begin
    if Tabs.ActivePage = TasksPage then
      TaskGrid.LoadFromCSVFile(OpenDialog.FileName)
    else if Tabs.ActivePage = StaticPage then
      StaticGrid.LoadFromCSVFile(OpenDialog.FileName)
    else if Tabs.ActivePage =  RewardsPage then
      RewardsGrid.LoadFromCSVFile(OpenDialog.FileName)
    else
      UpdateStatus('Please switch to the tab you wish to import to.');
  end;
end;

procedure TTodoForm.NewFileClick(Sender: TObject);
begin
  ClearAllData;
  Caption:=SettingsForm.ListName.Text+#39's Todo List';
end;

procedure TTodoForm.OpenFileClick(Sender: TObject);
begin
  {$IFDEF UNIX}
  OpenDialog.InitialDir:=GetEnvironmentVariable('HOME');
  {$ENDIF}
  OpenDialog.DefaultExt:='';
  OpenDialog.Filter:='';
  if OpenDialog.Execute then
    Try
      LoadFromFile(OpenDialog.FileName);
    Except
      On EInvalidFile do UpdateStatus('Attempted to load incorrect file format.');
    end;
end;

procedure TTodoForm.PurgeMenuClick(Sender: TObject);
begin
  if Tabs.ActivePage = TasksPage then
    TaskGrid.DeleteRow(TaskGrid.Row)
  else if Tabs.ActivePage = StaticPage then
    StaticGrid.DeleteRow(StaticGrid.Row)
  else if Tabs.ActivePage = RewardsPage then
    RewardsGrid.DeleteRow(RewardsGrid.Row);
  if SettingsForm.AutoSave.Checked then
    if FFileName <> '' then
      SaveToFile(FFileName);
end;

procedure TTodoForm.RewardsGridButtonClick(Sender: TObject; aCol, aRow: Integer
  );
var
  remaining: integer;
begin
  remaining:=FEarned-FUsed;
  if remaining < StrToInt(RewardsGrid.Cells[0, aRow]) then
  begin
    ShowMessage('Cannot redeem, not enough points.');
    AddToLog('Unable to redeem reward, not enough points.');
    Exit;
  end;
  Inc(FUsed, StrToInt(RewardsGrid.Cells[0, aRow]));
  Dec(FCurSession, StrToInt(RewardsGrid.Cells[0, aRow]));
  UpdatePoints;
  FModified:=True;
  UpdateStatus('Reward claimed: '+RewardsGrid.Cells[1, aRow]);
  Timer.Enabled:=True;
  AddToLog(StatusBar.Panels.Items[0].Text);
end;

procedure TTodoForm.SaveAsFileClick(Sender: TObject);
begin
  {$IFDEF UNIX}
  SaveDialog.InitialDir:=GetEnvironmentVariable('HOME');
  {$ENDIF}
  SaveDialog.DefaultExt:='';
  SaveDialog.Filter:='';
  if SaveDialog.Execute then
    SaveToFile(SaveDialog.FileName);
end;

procedure TTodoForm.SaveFileClick(Sender: TObject);
begin
  if FFileName = '' then
    SaveAsFileClick(Sender)
  else
    SaveToFile(FFileName);
end;

procedure TTodoForm.SettingsMenuClick(Sender: TObject);
var
  settings: TTodoSettings;
begin
  settings.list_name:=SettingsForm.ListName.Text;
  settings.priority:=StrToInt(SettingsForm.DefaultPriority.Text);
  settings.auto_save:=SettingsForm.AutoSave.Checked;
  if SettingsForm.ShowModal = mrOK then
  begin
    if FFileName = '' then
      Caption:=SettingsForm.ListName.Text+#39's Todo List'
    else
      Caption:=SettingsForm.ListName.Text+#39's Todo List // '+FFileName;
  end
  else
  begin
    SettingsForm.ListName.Text:=settings.list_name;
    SettingsForm.DefaultPriority.Text:=IntToStr(settings.priority);
    SettingsForm.AutoSave.Checked:=settings.auto_save;
  end;
end;

procedure TTodoForm.StaticGridButtonClick(Sender: TObject; aCol, aRow: Integer);
begin
  Inc(FEarned, StrToInt(StaticGrid.Cells[0, aRow]));
  Inc(FCurSession, StrToInt(StaticGrid.Cells[0, aRow]));
  UpdatePoints;
  FModified:=True;
  UpdateStatus('Static Task completed: '+StaticGrid.Cells[1, aRow]);
  Timer.Enabled:=True;
  AddToLog(StatusBar.Panels.Items[0].Text);
end;

procedure TTodoForm.TabsChange(Sender: TObject);
begin
  if Tabs.ActivePage = TasksPage then
    UpdateMenu('Tasks')
  else if Tabs.ActivePage = StaticPage then
    UpdateMenu('Static Tasks')
  else if Tabs.ActivePage = RewardsPage then
    UpdateMenu('Rewards');
end;

procedure TTodoForm.TaskGridClick(Sender: TObject);
begin
  if TaskGrid.Col <> 2 then
    Exit;
  if TaskGrid.Cells[2, TaskGrid.Row] = '0' then
  begin
    Inc(FEarned, StrToInt(TaskGrid.Cells[0, TaskGrid.Row]));
    Inc(FCompleted);
    Inc(FCurSession, StrToInt(TaskGrid.Cells[0, TaskGrid.Row]));
    UpdatePoints;
    TaskGrid.Cells[2, TaskGrid.Row] := '1';
    FModified:=True;
    UpdateStatus('Task completed: '+TaskGrid.Cells[1, TaskGrid.Row]);
    Timer.Enabled:=True;
    AddToLog('Task completed: '+TaskGrid.Cells[1, TaskGrid.Row]);
  end;
end;

procedure TTodoForm.TimerTimer(Sender: TObject);
begin
  Timer.Enabled:=False;
  UpdateStatus('Ready.');
  if FModified and SettingsForm.AutoSave.Checked then
    if FFileName <> '' then
      SaveToFile(FFileName);
end;

procedure TTodoForm.AdjustUI;
begin
  Tabs.Width:=ClientWidth;
  Tabs.Height:=ClientHeight-StatusBar.Height;
  TasksPage.Width:=tabs.ClientWidth;
  TasksPage.Height:=tabs.ClientHeight;
  TaskGrid.Width:=TasksPage.Width;
  TaskGrid.Height:=TasksPage.Height;
  TaskGrid.Columns.Items[1].Width:=TaskGrid.Width-90;
  StaticPage.Width:=tabs.ClientWidth;
  StaticPage.Height:=tabs.ClientHeight;
  StaticGrid.Width:=StaticPage.Width;
  StaticGrid.Height:=StaticPage.Height;
  StaticGrid.Columns.Items[1].Width:=StaticGrid.Width-90;
  RewardsPage.Width:=tabs.ClientWidth;
  RewardsPage.Height:=tabs.ClientHeight;
  RewardsGrid.Width:=RewardsPage.Width;
  RewardsGrid.Height:=RewardsPage.Height;
  RewardsGrid.Columns.Items[1].Width:=RewardsGrid.Width-90;
  LogPage.Width:=tabs.ClientWidth;
  LogPage.Height:=tabs.ClientHeight;
  TodoLog.Width:=LogPage.ClientWidth;
  TodoLog.Height:=LogPage.ClientHeight-24;
  StatusBar.Panels.Items[0].Width:=ClientWidth-100;
end;

procedure TTodoForm.AddTask(points, title: string);
begin
  TaskGrid.InsertRowWithValues(TaskGrid.RowCount, [points, title, '0']);
  Tabs.ActivePage:=TasksPage;
  FModified:=True;
  AddToLog('Add new Task: '+title);
end;

procedure TTodoForm.AddStaticTask(points, title: string);
begin
  StaticGrid.InsertRowWithValues(StaticGrid.RowCount, [points, title, 'X']);
  Tabs.ActivePage:=StaticPage;
  FModified:=True;
  AddToLog('Added new Static Task: '+title);
end;

procedure TTodoForm.AddReward(points, title: string);
begin
  RewardsGrid.InsertRowWithValues(RewardsGrid.RowCount, [points, title, 'X']);
  Tabs.ActivePage:=RewardsPage;
  FModified:=True;
  AddToLog('Added new Reward: '+title);
end;

procedure TTodoForm.UpdatePoints;
begin
  PointsEarned.Caption:=IntToStr(FEarned);
  PointsUsed.Caption:=IntToStr(FUsed);
  PointsRemaining.Caption:=IntToStr(FEarned-FUsed);
  TaskCount.Caption:=IntToStr(FCompleted)+' / '+IntToStr(TaskGrid.RowCount-1);
  CurSessPt.Caption:=IntToStr(FCurSession);
end;

procedure TTodoForm.ClearAllData;
var
  i: integer;
begin
  for i:=1 to TaskGrid.RowCount-1 do
    TaskGrid.DeleteRow(1);
  for i:=1 to StaticGrid.RowCount-1 do
    StaticGrid.DeleteRow(1);
  for i:=1 to RewardsGrid.RowCount-1 do
    RewardsGrid.DeleteRow(1);
  FEarned:=0;
  FUsed:=0;
  FCompleted:=0;
  FCurSession:=0;
  UpdatePoints;
  FModified:=False;
  FFileName:='';
  TodoLog.Lines.Clear;
  SettingsForm.ResetSettings;
end;

procedure TTodoForm.LoadDefaultFile;
var
  fname: string;
begin
  {$IFDEF UNIX}
  fname:=GetEnvironmentVariable('HOME')+DEFAULT_FILE;
  {$ELSE}
  fname:='tasks.dat';
  {$ENDIF}
  if not FileExists(fname) then
    Exit;
  Try
    LoadFromFile(fname);
  Except
    On EInvalidFile do UpdateStatus('Incorrect Todo file format.');
  end;
end;

procedure TTodoForm.LoadFromFile(fname: string);
var
  s: TMemoryStream;
  hdr: TTodoHeader;
  settings: TTodoSettings;
  task: TTodoItem;
  stask: TStaticItem;
  reward: TTodoReward;
  i: integer;
  v: string[1];
begin
  ClearAllData;
  s:=TMemoryStream.Create;
  try
    s.LoadFromFile(fname);
    FFileName:=fname;
    s.Read(hdr, SizeOf(hdr));
    if hdr.sig <> TD_SIG then
      Raise EInvalidFile.Create('Todo File Header incorrect.');
    if hdr.ver = 1 then
      AddToLog('Upgraded file version in memory.')
    else if hdr.ver = 2 then
    begin
      s.Read(settings, SizeOf(settings));
      SettingsForm.ListName.Text:=settings.list_name;
      SettingsForm.DefaultPriority.Text:=IntToStr(settings.priority);
      SettingsForm.AutoSave.Checked:=settings.auto_save;
      case settings.last_tab of
        0: Tabs.ActivePage:=TasksPage;
        1: Tabs.ActivePage:=StaticPage;
        2: Tabs.ActivePage:=RewardsPage;
        3: Tabs.ActivePage:=StatsPage;
        4: Tabs.ActivePage:=TasksPage;
      end;
    end
    else
      Raise EInvalidFile.Create('Todo File Version incorrect.');
    FEarned:=hdr.points;
    FUsed:=hdr.used;
    for i:=1 to hdr.tasks do
    begin
      s.Read(task, SizeOf(task));
      if task.done then
      begin
        v:='1';
        Inc(FCompleted);
      end
      else
        v:='0';
      TaskGrid.InsertRowWithValues(i, [IntToStr(task.priority), task.title, v]);
    end;
    for i:=1 to hdr.stasks do
    begin
      s.Read(stask, SizeOf(stask));
      StaticGrid.InsertRowWithValues(i, [IntToStr(stask.points), stask.title, 'X']);
    end;
    for i:=1 to hdr.rewards do
    begin
      s.Read(reward, SizeOf(reward));
      RewardsGrid.InsertRowWithValues(i, [IntToStr(reward.cost), reward.title, 'X']);
    end;
    UpdatePoints;
    UpdateStatus('Loaded from file: '+FFileName);
    Caption:=SettingsForm.ListName.Text+#39's Todo List // '+FFileName;
    Timer.Enabled:=True;
  finally
    s.Free;
  end;
end;

procedure TTodoForm.SaveToFile(fname: string);
var
  s: TMemoryStream;
  hdr: TTodoHeader;
  settings: TTodoSettings;
  task: TTodoItem;
  stask: TStaticItem;
  reward: TTodoReward;
  i: integer;
begin
  s:=TMemoryStream.Create;
  try
    hdr.sig:=TD_SIG;
    hdr.ver:=TD_VER;
    hdr.tasks:=TaskGrid.RowCount-1;
    hdr.stasks:=StaticGrid.RowCount-1;
    hdr.rewards:=RewardsGrid.RowCount-1;
    hdr.points:=FEarned;
    hdr.used:=FUsed;
    s.Write(hdr, SizeOf(hdr));
    settings.list_name:=SettingsForm.ListName.Text;
    settings.priority:=StrToInt(SettingsForm.DefaultPriority.Text);
    settings.auto_save:=SettingsForm.AutoSave.Checked;
    if Tabs.ActivePage = TasksPage then
      settings.last_tab:=0
    else if Tabs.ActivePage = StaticPage then
      settings.last_tab:=1
    else if Tabs.ActivePage = RewardsPage then
      settings.last_tab:=2
    else if Tabs.ActivePage = StatsPage then
      settings.last_tab:=3
    else if Tabs.ActivePage = LogPage then
      settings.last_tab:=4;
    s.Write(settings, SizeOf(settings));
    for i:=1 to hdr.tasks do
    begin
      task.priority:=StrToInt(TaskGrid.Cells[0, i]);
      task.title:=TaskGrid.Cells[1, i];
      if TaskGrid.Cells[2, i] = '0' then
        task.done:=False
      else
        task.done:=True;
      s.Write(task, SizeOf(task));
    end;
    for i:=1 to hdr.stasks do
    begin
      stask.points:=StrToInt(StaticGrid.Cells[0, i]);
      stask.title:=StaticGrid.Cells[1, i];
      s.Write(stask, SizeOf(stask));
    end;
    for i:=1 to hdr.rewards do
    begin
      reward.cost:=StrToInt(RewardsGrid.Cells[0, i]);
      reward.title:=RewardsGrid.Cells[1, i];
      s.Write(reward, SizeOf(reward));
    end;
    s.SaveToFile(fname);
    FFileName:=fname;
    UpdateStatus('Saved to file: '+FFileName);
    Caption:=SettingsForm.ListName.Text+#39's Todo List // '+FFileName;
    Timer.Enabled:=True;
  finally
    s.Free;
  end;
  FModified:=False;
end;

procedure TTodoForm.ArchiveTask(points, title: string);
begin
  TaskGrid.InsertRowWithValues(TaskGrid.RowCount, [points, title, '1']);
  Inc(FEarned, StrToInt(points));
  Inc(FCompleted);
  Inc(FCurSession, StrToInt(points));
end;

procedure TTodoForm.UpdateMenu(TabName: string);
begin
  ExportMenu.Caption:='Export '+TabName+' to CSV...';
  ImportMenu.Caption:='Import '+TabName+' from CSV...';
end;

procedure TTodoForm.AddToLog(const msg: string);
begin
  TodoLog.Lines.Add(msg);
  TodoLog.SelStart:=TodoLog.GetTextLen;
end;

procedure TTodoForm.UpdateStatus(const msg: string);
begin
  StatusBar.Panels.Items[0].Text:=msg;
  StatusBar.Panels.Items[1].Text:='Points: '+IntToStr(FEarned-FUsed);
end;

end.

