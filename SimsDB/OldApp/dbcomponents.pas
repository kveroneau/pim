unit dbcomponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSimRole = (srFounder, srSpouse, srFather, srMother, srSon, srDaughter);
  TLifeStage = (lsBaby, lsToddler, lsChild, lsTeen, lsYoungAdult, lsAdult,
                lsElder, lsDead);

  TSim = class(TComponent)
  private
    FFirstName: String[40];
    FLastName: String[40];
    FRole: TSimRole;
    FLifeStage: TLifeStage;
  end;

implementation

end.

