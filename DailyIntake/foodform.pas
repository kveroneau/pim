unit foodform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, intakedb;

type

  { TFoodDialog }

  TFoodDialog = class(TForm)
    SaveBtn: TButton;
    CloseBtn: TButton;
    Carbs: TEdit;
    Potassium: TEdit;
    Protein: TEdit;
    Fiber: TEdit;
    Sodium: TEdit;
    Sugar: TEdit;
    FoodName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
  private

  public
    procedure PopulateForm(food: PFood);
    procedure GrabData(food: PFood);
  end;

var
  FoodDialog: TFoodDialog;

implementation

{$R *.lfm}

{ TFoodDialog }

procedure TFoodDialog.PopulateForm(food: PFood);
begin
  FoodName.Text:=food^.name;
  Carbs.Text:=IntToStr(food^.carbs);
  Sugar.Text:=IntToStr(food^.sugar);
  Sodium.Text:=IntToStr(food^.sodium);
  Fiber.Text:=IntToStr(food^.fiber);
  Protein.Text:=IntToStr(food^.protein);
  Potassium.Text:=IntToStr(food^.potassium);
end;

procedure TFoodDialog.GrabData(food: PFood);
begin
  food^.name:=FoodName.Text;
  food^.carbs:=StrToInt(Carbs.Text);
  food^.sugar:=StrToInt(Sugar.Text);
  food^.sodium:=StrToInt(Sodium.Text);
  food^.fiber:=StrToInt(Fiber.Text);
  food^.protein:=StrToInt(Protein.Text);
  food^.potassium:=StrToInt(Potassium.Text);
end;

end.

