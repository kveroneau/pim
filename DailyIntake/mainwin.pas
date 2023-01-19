unit mainwin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ComCtrls, intakedb, mealform, DateUtils;

type

  { TDailyIntakeForm }

  TDailyIntakeForm = class(TForm)
    ImageList: TImageList;
    DailyTracker: TStringGrid;
    MealList: TListBox;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ToolBar: TToolBar;
    NewBtn: TToolButton;
    OpenBtn: TToolButton;
    SaveBtn: TToolButton;
    ToolButton1: TToolButton;
    AddMealBtn: TToolButton;
    EditMealBtn: TToolButton;
    procedure AddMealBtnClick(Sender: TObject);
    procedure EditMealBtnClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MealListDblClick(Sender: TObject);
    procedure NewBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    FFoodItems: TFoodItems;
    FMeals: TMeals;
    FDays: TDailyMeals;
    FFileName: string;
    procedure AdjustUI;
    procedure UpdateRow(row_info: PTrackedDay);
  public

  end;

var
  DailyIntakeForm: TDailyIntakeForm;

implementation

{$R *.lfm}

{ TDailyIntakeForm }

procedure TDailyIntakeForm.NewBtnClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=1 to DailyTracker.RowCount-1 do
    DailyTracker.DeleteRow(1);
  MealList.Items.Clear;
  SetLength(FFoodItems, 0);
  SetLength(FMeals, 0);
  SetLength(FDays, 0);
  FFileName:='';
end;

procedure TDailyIntakeForm.AddMealBtnClick(Sender: TObject);
var
  prior: word;
  r: TModalResult;
begin
  prior:=High(FMeals)+1;
  SetLength(FMeals, High(FMeals)+2);
  MealDialog.PopulateForm(@FMeals[High(FMeals)], @FFoodItems);
  r:=MealDialog.ShowModal;
  if r = mrOK then
  begin
    MealDialog.GrabData(@FMeals[High(FMeals)]);
    MealList.Items.Add(FMeals[High(FMeals)].name);
  end
  else
    SetLength(FMeals, prior);
end;

procedure TDailyIntakeForm.EditMealBtnClick(Sender: TObject);
var
  r: TModalResult;
begin
  MealDialog.PopulateForm(@FMeals[MealList.ItemIndex], @FFoodItems);
  r:=MealDialog.ShowModal;
  if r = mrOK then
  begin
    MealDialog.GrabData(@FMeals[MealList.ItemIndex]);
    MealList.Items.Strings[MealList.ItemIndex]:=FMeals[MealList.ItemIndex].name;
  end;
end;

procedure TDailyIntakeForm.FormResize(Sender: TObject);
begin
  AdjustUI;
end;

procedure TDailyIntakeForm.MealListDblClick(Sender: TObject);
begin
  SetLength(FDays, High(FDays)+2);
  with FDays[High(FDays)] do
  begin
    { TODO : Properly check if day exists and add meal accordingly }
    day:=Today;
    FillByte(meals, 10, 0);
    meals[0]:=MealList.ItemIndex+1;
    GetMealData(@FMeals[MealList.ItemIndex], @FFoodItems, carbs, sugar, sodium,
        fiber, protein, potassium);
  end;
  UpdateRow(@FDays[High(FDays)]);
end;

procedure TDailyIntakeForm.OpenBtnClick(Sender: TObject);
var
  i: Integer;
begin
  {$IFDEF DEBUG}
  OpenDialog.InitialDir:=GetCurrentDir;
  {$ENDIF}
  if not OpenDialog.Execute then
    Exit;
  NewBtnClick(Self);
  LoadIntakeDB(OpenDialog.FileName, @FFoodItems, @FMeals, @FDays);
  for i:=Low(FMeals) to High(FMeals) do
    MealList.Items.Add(FMeals[i].name);
  for i:=Low(FDays) to High(FDays) do
    UpdateRow(@FDays[i]);
  FFileName:=OpenDialog.FileName;
  Caption:='Daily Food Intake // '+FFileName;
end;

procedure TDailyIntakeForm.SaveBtnClick(Sender: TObject);
var
  i: Integer;
begin
  {$IFDEF DEBUG}
  SaveDialog.InitialDir:=GetCurrentDir;
  {$ENDIF}
  if FFileName = '' then
    if not SaveDialog.Execute then
      Exit;
  SaveIntakeDB(SaveDialog.FileName, @FFoodItems, @FMeals, @FDays);
  FFileName:=SaveDialog.FileName;
  Caption:='Daily Food Intake // '+FFileName;
end;

procedure TDailyIntakeForm.AdjustUI;
begin
  DailyTracker.Height:=ClientHeight-ToolBar.Height;
  MealList.Height:=DailyTracker.Height;
  MealList.Width:=ClientWidth-DailyTracker.Width;
end;

procedure TDailyIntakeForm.UpdateRow(row_info: PTrackedDay);
var
  row: Integer;
  row_day: TDate;
begin
  row:=1;
  if (DailyTracker.RowCount > 1) and TryISO8601ToDate(DailyTracker.Cells[0,1], row_day) then
    if CompareDate(row_day, row_info^.day) = 0 then
    begin
      with row_info^ do
      begin
        DailyTracker.Cells[1,1]:=IntToStr(carbs);
        DailyTracker.Cells[2,1]:=IntToStr(sugar);
        DailyTracker.Cells[3,1]:=IntToStr(sodium);
        DailyTracker.Cells[4,1]:=IntToStr(fiber);
        DailyTracker.Cells[5,1]:=IntToStr(protein);
        DailyTracker.Cells[6,1]:=IntToStr(potassium);
      end;
      Exit;
    end;
  with row_info^ do
    DailyTracker.InsertRowWithValues(row, [DateToISO8601(day),
      IntToStr(carbs), IntToStr(sugar), IntToStr(sodium), IntToStr(fiber),
      IntToStr(protein), IntToStr(potassium)]);
end;

end.

