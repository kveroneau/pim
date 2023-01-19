unit mainwin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, intakedb,
  foodform, mealform;

type

  { TDailyIntakeForm }

  TDailyIntakeForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FFoodItems: TFoodItems;
    FMeals: TMeals;
  public

  end;

var
  DailyIntakeForm: TDailyIntakeForm;

implementation

{$R *.lfm}

{ TDailyIntakeForm }

procedure TDailyIntakeForm.Button1Click(Sender: TObject);
begin
  SetLength(FFoodItems, 2);
  SetLength(FMeals, 2);
  with FFoodItems[0] do
  begin
    name:='Cream';
    carbs:=0;
    sugar:=0;
    sodium:=40;
    fiber:=0;
    protein:=4;
    potassium:=10;
  end;
  with FFoodItems[1] do
  begin
    name:='Milk';
    carbs:=4;
    sugar:=2;
    sodium:=10;
    fiber:=0;
    protein:=6;
    potassium:=20;
  end;
  with FMeals[0] do
  begin
    name:='Something';
    FillByte(food, 20, 0);
    food[0]:=1;
  end;
  with FMeals[1] do
  begin
    name:='Something else';
    FillByte(food, 20, 0);
    food[0]:=1;
    food[1]:=2;
  end;
  SaveIntakeDB('intake.db', @FFoodItems, @FMeals);
  SetLength(FFoodItems, 0);
  SetLength(FMeals, 0);
end;

procedure TDailyIntakeForm.Button2Click(Sender: TObject);
var
  r: TModalResult;
begin
  LoadIntakeDB('intake2.db', @FFoodItems, @FMeals);
  MealDialog.PopulateForm(@FMeals[0], @FFoodItems);
  r:=MealDialog.ShowModal;
  if r = mrOK then
    MealDialog.GrabData(@FMeals[0]);
  SaveIntakeDB('intake2.db', @FFoodItems, @FMeals);
  SetLength(FFoodItems, 0);
  SetLength(FMeals, 0);
end;

end.

