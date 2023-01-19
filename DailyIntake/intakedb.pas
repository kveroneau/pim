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

  EInvalidFile = class(Exception);

procedure SaveIntakeDB(const FileName: string; food_list: PFoodItems;
  meal_list: PMeals);
procedure LoadIntakeDB(const FileName: string; food_list: PFoodItems;
  meal_list: PMeals);

implementation

type
  TSignature = Array[0..3] of char;
  TIntakeHeader = packed record
    sig: TSignature;
    version: byte;
    food, meals: byte;
  end;

const
  INTAKE_SIG: TSignature = ('D','F','I','*');
  INTAKE_VER = 1;

procedure SaveIntakeDB(const FileName: string; food_list: PFoodItems;
  meal_list: PMeals);
var
  f: TMemoryStream;
  i: integer;
  hdr: TIntakeHeader;
begin
  hdr.sig:=INTAKE_SIG;
  hdr.version:=INTAKE_VER;
  hdr.food:=High(food_list^)+1;
  hdr.meals:=High(meal_list^)+1;
  f:=TMemoryStream.Create;
  try
    f.Write(hdr, SizeOf(hdr));
    for i:=Low(food_list^) to High(food_list^) do
      f.Write(food_list^[i], SizeOf(TFood));
    for i:=Low(meal_list^) to High(meal_list^) do
      f.Write(meal_list^[i], SizeOf(TMeal));
    f.SaveToFile(FileName);
  finally
    f.Free;
  end;
end;

procedure LoadIntakeDB(const FileName: string; food_list: PFoodItems;
  meal_list: PMeals);
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
    for i:=Low(food_list^) to High(food_list^) do
      f.Read(food_list^[i], SizeOf(TFood));
    for i:=Low(meal_list^) to High(meal_list^) do
      f.Read(meal_list^[i], SizeOf(TMeal));
  finally
    f.Free;
  end;
end;

end.

