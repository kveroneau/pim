unit todolist;

{ Built in 3 hours! }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls, Grids,
  StdCtrls, ExtCtrls, newtask;

type

  { TTodoForm }

  TTodoForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    HelpMenu: TMenuItem;
    AboutMenu: TMenuItem;
    SaveAsFile: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
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
    procedure ExitAppClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure NewFileClick(Sender: TObject);
    procedure OpenFileClick(Sender: TObject);
    procedure RewardsGridButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure SaveAsFileClick(Sender: TObject);
    procedure SaveFileClick(Sender: TObject);
    procedure StaticGridButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure TaskGridClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FEarned, FUsed, FCompleted: Integer;
    FModified: Boolean;
    FFileName: string;
    procedure AdjustUI;
    procedure AddTask(points, title: string);
    procedure AddStaticTask(points, title: string);
    procedure AddReward(points, title: string);
    procedure UpdatePoints;
    procedure ClearAllData;
    procedure LoadDefaultFile;
    procedure LoadFromFile(fname: string);
    procedure SaveToFile(fname: string);
  public

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
  APP_CAPTION = 'Kevin''s Todo List';
  TD_SIG: TSignature = ('K','T','D','*');
  TD_VER = 1;

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
  FModified:=False;
  FEarned:=0;
  FUsed:=0;
  FCompleted:=0;
  UpdatePoints;
  FFileName:='';
  StatusBar.SimpleText:='Ready';
  if ParamCount = 1 then
    Try
      LoadFromFile(ParamStr(1))
    Except
      On EInvalidFile do StatusBar.SimpleText:='Error loading '+ParamStr(1);
    end
  else
    LoadDefaultFile;
end;

procedure TTodoForm.AddTaskMenuClick(Sender: TObject);
var
  twin: TNewTaskForm;
  r: Integer;
begin
  twin:=TNewTaskForm.Create(Nil);
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
end;

procedure TTodoForm.AboutMenuClick(Sender: TObject);
begin
  ShowMessage('Kevin''s Todo List Application v0.2');
end;

procedure TTodoForm.ExitAppClick(Sender: TObject);
begin
  Close;
end;

procedure TTodoForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  {$IFDEF DEBUG}
  Exit;
  {$ENDIF}
  if FModified then
    if FFileName <> '' then
      SaveToFile(FFileName)
    else
      CloseAction:=caNone;
end;

procedure TTodoForm.FormResize(Sender: TObject);
begin
  AdjustUI;
end;

procedure TTodoForm.NewFileClick(Sender: TObject);
begin
  ClearAllData;
  Caption:=APP_CAPTION;
end;

procedure TTodoForm.OpenFileClick(Sender: TObject);
begin
  {$IFDEF UNIX}
  OpenDialog.InitialDir:=GetEnvironmentVariable('HOME');
  {$ENDIF}
  if OpenDialog.Execute then
    Try
      LoadFromFile(OpenDialog.FileName);
    Except
      On EInvalidFile do StatusBar.SimpleText:='Attempted to load incorrect file format.';
    end;
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
    Exit;
  end;
  Inc(FUsed, StrToInt(RewardsGrid.Cells[0, aRow]));
  UpdatePoints;
  FModified:=True;
  StatusBar.SimpleText:='Reward claimed: '+RewardsGrid.Cells[1, aRow];
  Timer.Enabled:=True;
end;

procedure TTodoForm.SaveAsFileClick(Sender: TObject);
begin
  {$IFDEF UNIX}
  SaveDialog.InitialDir:=GetEnvironmentVariable('HOME');
  {$ENDIF}
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

procedure TTodoForm.StaticGridButtonClick(Sender: TObject; aCol, aRow: Integer);
begin
  Inc(FEarned, StrToInt(StaticGrid.Cells[0, aRow]));
  UpdatePoints;
  FModified:=True;
  StatusBar.SimpleText:='Static Task completed: '+StaticGrid.Cells[1, aRow];
  Timer.Enabled:=True;
end;

procedure TTodoForm.TaskGridClick(Sender: TObject);
begin
  if TaskGrid.Col <> 2 then
    Exit;
  if TaskGrid.Cells[2, TaskGrid.Row] = '0' then
  begin
    Inc(FEarned, StrToInt(TaskGrid.Cells[0, TaskGrid.Row]));
    Inc(FCompleted);
    UpdatePoints;
    TaskGrid.Cells[2, TaskGrid.Row] := '1';
    FModified:=True;
    StatusBar.SimpleText:='Task completed: '+TaskGrid.Cells[1, TaskGrid.Row];
    Timer.Enabled:=True;
  end;
end;

procedure TTodoForm.TimerTimer(Sender: TObject);
begin
  Timer.Enabled:=False;
  StatusBar.SimpleText:='Ready.';
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
end;

procedure TTodoForm.AddTask(points, title: string);
begin
  TaskGrid.InsertRowWithValues(TaskGrid.RowCount, [points, title, '0']);
  Tabs.ActivePage:=TasksPage;
  FModified:=True;
end;

procedure TTodoForm.AddStaticTask(points, title: string);
begin
  StaticGrid.InsertRowWithValues(StaticGrid.RowCount, [points, title, 'X']);
  Tabs.ActivePage:=StaticPage;
  FModified:=True;
end;

procedure TTodoForm.AddReward(points, title: string);
begin
  RewardsGrid.InsertRowWithValues(RewardsGrid.RowCount, [points, title, 'X']);
  Tabs.ActivePage:=RewardsPage;
  FModified:=True;
end;

procedure TTodoForm.UpdatePoints;
begin
  PointsEarned.Caption:=IntToStr(FEarned);
  PointsUsed.Caption:=IntToStr(FUsed);
  PointsRemaining.Caption:=IntToStr(FEarned-FUsed);
  TaskCount.Caption:=IntToStr(FCompleted)+' / '+IntToStr(TaskGrid.RowCount-1);
end;

procedure TTodoForm.ClearAllData;
var
  i: integer;
begin
  for i:=1 to TaskGrid.RowCount-1 do
    TaskGrid.DeleteRow(i);
  for i:=1 to StaticGrid.RowCount-1 do
    StaticGrid.DeleteRow(i);
  for i:=1 to RewardsGrid.RowCount-1 do
    RewardsGrid.DeleteRow(i);
  FEarned:=0;
  FUsed:=0;
  FCompleted:=0;
  UpdatePoints;
  FModified:=False;
  FFileName:='';
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
    On EInvalidFile do StatusBar.SimpleText:='Incorrect Todo file format.';
  end;
end;

procedure TTodoForm.LoadFromFile(fname: string);
var
  s: TMemoryStream;
  hdr: TTodoHeader;
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
    if hdr.ver <> TD_VER then
      Raise EInvalidFile.Create('Todo File Version mismatch.');
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
    StatusBar.SimpleText:='Loaded from file: '+FFileName;
    Caption:=APP_CAPTION+' // '+FFileName;
    Timer.Enabled:=True;
  finally
    s.Free;
  end;
end;

procedure TTodoForm.SaveToFile(fname: string);
var
  s: TMemoryStream;
  hdr: TTodoHeader;
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
    StatusBar.SimpleText:='Saved to file: '+FFileName;
    Caption:=APP_CAPTION+' // '+FFileName;
    Timer.Enabled:=True;
  finally
    s.Free;
  end;
  FModified:=False;
end;

end.

