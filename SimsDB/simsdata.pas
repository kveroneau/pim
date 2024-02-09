unit simsdata;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, dbf, DB, simconsts;

type

  { TSimsData }

  TSimsData = class(TDataModule)
    FamilyDS: TDataSource;
    FamilyDBF: TDbf;
    LotDS: TDataSource;
    LotDBF: TDbf;
    ZodiacDS: TDataSource;
    ZodiacDBF: TDbf;
    StagesDS: TDataSource;
    StagesDBF: TDbf;
    SimDS: TDataSource;
    SimDBF: TDbf;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    procedure GenerateStageDataset;
    procedure GenerateZodiacDataset;
  public
    procedure JumpToSim(SimID: Integer);
  end;

var
  SimsDatabase: TSimsData;

implementation

{$R *.lfm}

{ TSimsData }

procedure TSimsData.DataModuleCreate(Sender: TObject);
begin
  with StagesDBF.FieldDefs do
  begin
    Add('Id', ftInteger, 0, True);
    Add('LifeStage', ftString, 20, True);
  end;
  StagesDBF.Active:=True;
  if StagesDBF.EOF then
    GenerateStageDataset;
  with ZodiacDBF.FieldDefs do
  begin
    Add('Id', ftInteger, 0, True);
    Add('ZodiacSign', ftString, 20, True);
  end;
  ZodiacDBF.Active:=True;
  if ZodiacDBF.EOF then
    GenerateZodiacDataset;
end;

procedure TSimsData.DataModuleDestroy(Sender: TObject);
begin
  ZodiacDBF.Active:=False;
  StagesDBF.Active:=False;
  SimDBF.Active:=False;
end;

procedure TSimsData.GenerateStageDataset;
var
  i: integer;
begin
  with StagesDBF do
    for i:=0 to Length(SIM_LIFE_STAGES)-1 do
    begin
      Append;
      FieldByName('Id').AsInteger:=i;
      FieldByName('LifeStage').AsString:=SIM_LIFE_STAGES[i];
      Post;
    end;
end;

procedure TSimsData.GenerateZodiacDataset;
var
  i: Integer;
begin
  with ZodiacDBF do
    for i:=0 to Length(SIM_ZODIAC_SIGNS)-1 do
    begin
      Append;
      FieldByName('Id').AsInteger:=i;
      FieldByName('ZodiacSign').AsString:=SIM_ZODIAC_SIGNS[i];
      Post;
    end;
end;

procedure TSimsData.JumpToSim(SimID: Integer);
var
  flt: string;
begin
  flt:=SimDBF.Filter;
  SimDBF.Filter:='ID='+IntToStr(SimID);
  SimDBF.Filtered:=True;
  SimDBF.Filtered:=False;
  SimDBF.Filter:=flt;
end;

end.

