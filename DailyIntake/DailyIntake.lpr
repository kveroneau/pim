program DailyIntake;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainwin, intakedb, foodform, mealform
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Daily Food Intake';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDailyIntakeForm, DailyIntakeForm);
  Application.CreateForm(TFoodDialog, FoodDialog);
  Application.CreateForm(TMealDialog, MealDialog);
  Application.Run;
end.

