unit mealform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, intakedb,
  foodform;

type

  { TMealDialog }

  TMealDialog = class(TForm)
    AddBtn: TButton;
    Carbs: TLabel;
    Fiber: TLabel;
    Potassium: TLabel;
    Protein: TLabel;
    Sodium: TLabel;
    Sugar: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MealCalc: TGroupBox;
    NewBtn: TButton;
    SaveBtn: TButton;
    CloseBtn: TButton;
    RemoveBtn: TButton;
    Label2: TLabel;
    Label3: TLabel;
    FoodList: TListBox;
    MealContent: TListBox;
    MealName: TEdit;
    Label1: TLabel;
    procedure AddBtnClick(Sender: TObject);
    procedure FoodListDblClick(Sender: TObject);
    procedure NewBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
  private
    FMeal: PMeal;
    FFoodList: PFoodItems;
    procedure Calculate;
  public
    procedure PopulateForm(meal: PMeal; food_list: PFoodItems);
    procedure GrabData(meal: PMeal);
  end;

var
  MealDialog: TMealDialog;

implementation

{$R *.lfm}

{ TMealDialog }

procedure TMealDialog.AddBtnClick(Sender: TObject);
var
  i: Integer;
begin
  i:=MealContent.Items.Add(FoodList.GetSelectedText);
  FMeal^.food[i]:=FoodList.ItemIndex+1;
  Calculate;
end;

procedure TMealDialog.FoodListDblClick(Sender: TObject);
var
  r: TModalResult;
begin
  FoodDialog.PopulateForm(@FFoodList^[FoodList.ItemIndex]);
  r:=FoodDialog.ShowModal;
  if r = mrOK then
  begin
    FoodDialog.GrabData(@FFoodList^[FoodList.ItemIndex]);
    FoodList.Items.Strings[FoodList.ItemIndex]:=FFoodList^[FoodList.ItemIndex].name;
  end;
end;

procedure TMealDialog.NewBtnClick(Sender: TObject);
var
  r: TModalResult;
  prior: byte;
begin
  prior:=High(FFoodList^)+1;
  SetLength(FFoodList^, High(FFoodList^)+2);
  FoodDialog.PopulateForm(@FFoodList^[High(FFoodList^)]);
  r:=FoodDialog.ShowModal;
  if r = mrOK then
  begin
    FoodDialog.GrabData(@FFoodList^[High(FFoodList^)]);
    FoodList.Items.Add(FFoodList^[High(FFoodList^)].name)
  end
  else
    SetLength(FFoodList^, prior);
end;

procedure TMealDialog.RemoveBtnClick(Sender: TObject);
begin
  MealContent.Items.Delete(MealContent.ItemIndex);
  GrabData(FMeal);
  Calculate;
end;

procedure TMealDialog.Calculate;
var
  o_carbs, o_sugar, o_sodium, o_fiber, o_protein, o_potassium: word;
begin
  GetMealData(FMeal, FFoodList, o_carbs, o_sugar, o_sodium, o_fiber, o_protein,
    o_potassium);
  Carbs.Caption:=IntToStr(o_carbs);
  Sugar.Caption:=IntToStr(o_sugar);
  Sodium.Caption:=IntToStr(o_sodium);
  Fiber.Caption:=IntToStr(o_fiber);
  Protein.Caption:=IntToStr(o_protein);
  Potassium.Caption:=IntToStr(o_potassium);
end;

procedure TMealDialog.PopulateForm(meal: PMeal; food_list: PFoodItems);
var
  i, f: Integer;
begin
  MealName.Text:=meal^.name;
  MealContent.Clear;
  FoodList.Items.Clear;
  FMeal:=meal;
  FFoodList:=food_list;
  for i:=Low(FFoodList^) to High(FFoodList^) do
    FoodList.Items.Add(FFoodList^[i].name);
  for i:=0 to High(FMeal^.food) do
    if FMeal^.food[i] > 0 then
    begin
      f:=FMeal^.food[i]-1;
      MealContent.Items.Add(FFoodList^[f].name);
    end;
  Calculate;
end;

procedure TMealDialog.GrabData(meal: PMeal);
var
  i: Integer;
begin
  meal^.name:=MealName.Text;
  FillByte(meal^.food, 20, 0);
  for i:=0 to MealContent.Items.Count-1 do
    meal^.food[i]:=FoodList.Items.IndexOf(MealContent.Items.Strings[i])+1;
end;

end.

