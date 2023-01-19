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
begin
  MealContent.Items.Add(FoodList.GetSelectedText);
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

