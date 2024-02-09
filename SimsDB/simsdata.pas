unit simsdata;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, dbf, DB, simconsts, fpjson, jsonparser;

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
    procedure SimDBFNewRecord(DataSet: TDataSet);
  private
    procedure GenerateStageDataset;
    procedure GenerateZodiacDataset;
    function ExportDBF(DataSet: TDataSet): TJSONObject;
  public
    procedure JumpToSim(SimID: Integer);
    function ExportData: TJSONObject;
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

procedure TSimsData.SimDBFNewRecord(DataSet: TDataSet);
var
  i: integer;
begin
  for i:=0 to Length(SIM_PERSONALITY)-1 do
    SimDBF.FieldByName(SIM_PERSONALITY[i]).AsInteger:=0;
  for i:=0 to Length(SIM_SKILLS)-1 do
    SimDBF.FieldByName(SIM_SKILLS[i]).AsInteger:=0;
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

function TSimsData.ExportData: TJSONObject;
var
  tbls: TJSONArray;
begin
  Result:=TJSONObject.Create;
  with SimsDatabase do
  begin
    Result.Add('sims', ExportDBF(SimDBF));
    Result.Add('lots', ExportDBF(LotDBF));
    Result.Add('families', ExportDBF(FamilyDBF));
  end;
  tbls:=TJSONArray.Create(['sims', 'lots', 'families']);
  Result.Add('tables', tbls);
end;

function TSimsData.ExportDBF(DataSet: TDataSet): TJSONObject;
var
  md, field: TJSONObject;
  data, fields: TJSONArray;
  i, oldrec: integer;
  fd: TFieldDef;
begin
  Result:=TJSONObject.Create;
  md:=TJSONObject.Create;
  data:=TJSONArray.Create;
  fields:=TJSONArray.Create;
  for i:=0 to DataSet.FieldCount-1 do
  begin
    fd:=DataSet.FieldDefs.Items[i];
    field:=TJSONObject.Create;
    field.Strings['name']:=fd.Name;
    if (fd.DataType = ftAutoInc) or (fd.DataType = ftInteger) then
      field.Strings['type']:='int'
    else if fd.DataType = ftString then
      field.Strings['type']:='string';
    fields.Add(field);
  end;
  md.Add('fields', fields);
  md.Add('root', 'Data');
  with DataSet do
  begin
    oldrec:=RecNo;
    First;
    repeat
      field:=TJSONObject.Create;
      for i:=0 to DataSet.FieldCount-1 do
      begin
        fd:=DataSet.FieldDefs.Items[i];
        if (fd.DataType = ftAutoInc) or (fd.DataType = ftInteger) then
          field.Add(fd.Name, DataSet.Fields.Fields[i].AsInteger)
        else if fd.DataType = ftString then
          field.Add(fd.Name, DataSet.Fields.Fields[i].AsString);
      end;
      data.Add(field);
      Next;
    until EOF;
    RecNo:=oldrec;
  end;
  Result.Add('metaData', md);
  Result.Add('Data', data);
end;

end.

