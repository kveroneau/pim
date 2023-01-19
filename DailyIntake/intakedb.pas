unit intakedb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  PFood = ^TFood;
  TFood = packed record
    name: string[30];
    carbs, sugar, sodium, fiber, protein, potassium: word;
  end;

  PMeal = ^TMeal;
  TMeal = packed record
    name: string[30];
    food: Array[0..20] of byte;
  end;

  PTrackedDay = ^TTrackedDay;
  TTrackedDay = packed record
    day: TDate;
    meals: Array[0..10] of byte; { Shouldn't need more than 10 meals? }
    carbs, sugar, sodium, fiber, protein, potassium: word;
  end;

  PFoodItems = ^TFoodItems;
  TFoodItems = Array of TFood;
  PMeals = ^TMeals;
  TMeals = Array of TMeal;
  PDailyMeals = ^TDailyMeals;
  TDailyMeals = Array of TTrackedDay;

  EInvalidFile = class(Exception);

procedure SaveIntakeDB(const FileName: string; food_list: PFoodItems;
  meal_list: PMeals; day_list: PDailyMeals);
procedure LoadIntakeDB(const FileName: string; food_list: PFoodItems;
  meal_list: PMeals; day_list: PDailyMeals);
procedure GetMealData(meal: PMeal; food_list: PFoodItems; out o_carbs, o_sugar,
  o_sodium, o_fiber, o_protein, o_potassium: word);

implementation

type
  TSignature = Array[0..3] of char;
  TIntakeHeader = packed record
    sig: TSignature;
    version: byte;
    food, meals, days: byte;
  end;

const
  INTAKE_SIG: TSignature = ('D','F','I','*');
  INTAKE_VER = 1;

procedure SaveIntakeDB(const FileName: string; food_list: PFoodItems;
  meal_list: PMeals; day_list: PDailyMeals);
var
  f: TMemoryStream;
  i: integer;
  hdr: TIntakeHeader;
begin
  hdr.sig:=INTAKE_SIG;
  hdr.version:=INTAKE_VER;
  hdr.food:=High(food_list^)+1;
  hdr.meals:=High(meal_list^)+1;
  hdr.days:=High(day_list^)+1;
  f:=TMemoryStream.Create;
  try
    f.Write(hdr, SizeOf(hdr));
    for i:=Low(food_list^) to High(food_list^) do
      f.Write(food_list^[i], SizeOf(TFood));
    for i:=Low(meal_list^) to High(meal_list^) do
      f.Write(meal_list^[i], SizeOf(TMeal));
    for i:=Low(day_list^) to High(day_list^) do
      f.Write(day_list^[i], SizeOf(TTrackedDay));
    f.SaveToFile(FileName);
  finally
    f.Free;
  end;
end;

procedure LoadIntakeDB(const FileName: string; food_list: PFoodItems;
  meal_list: PMeals; day_list: PDailyMeals);
var
  f: TMemoryStream;
  i: Integer;
  hdr: TIntakeHeader;
begin
  f:=TMemoryStream.Create;
  try
    f.LoadFromFile(FileName);
    f.Read(hdr, SizeOf(hdr));
    if hdr.sig <> INTAKE_SIG then
      raise EInvalidFile.Create('Not a valid Daily Food Intake DB!');
    if hdr.version <> INTAKE_VER then
      raise EInvalidFile.Create('IntakeDB version mismatch!');
    SetLength(food_list^, hdr.food);
    SetLength(meal_list^, hdr.meals);
    SetLength(day_list^, hdr.days);
    for i:=Low(food_list^) to High(food_list^) do
      f.Read(food_list^[i], SizeOf(TFood));
    for i:=Low(meal_list^) to High(meal_list^) do
      f.Read(meal_list^[i], SizeOf(TMeal));
    for i:=Low(day_list^) to High(day_list^) do
      f.Read(day_list^[i], SizeOf(TTrackedDay));
  finally
    f.Free;
  end;
end;

procedure GetMealData(meal: PMeal; food_list: PFoodItems; out o_carbs, o_sugar,
  o_sodium, o_fiber, o_protein, o_potassium: word);
var
  i: Integer;
begin
  o_carbs:=0;
  o_sugar:=0;
  o_sodium:=0;
  o_fiber:=0;
  o_protein:=0;
  o_potassium:=0;
  for i:=0 to High(meal^.food) do
    if meal^.food[i] > 0 then
      with food_list^[meal^.food[i]-1] do
      begin
        Inc(o_carbs, carbs);
        Inc(o_sugar, sugar);
        Inc(o_sodium, sodium);
        Inc(o_fiber, fiber);
        Inc(o_protein, protein);
        Inc(o_potassium, potassium);
      end;
end;

end.

