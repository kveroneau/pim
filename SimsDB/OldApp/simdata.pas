unit simdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls;

type

  { TSim }

  TSim = class(TFrame)
    FPartner: TComboBox;
    FSubHobby: TEdit;
    FHobby: TEdit;
    FJob: TEdit;
    FCareer: TComboBox;
    FLTW: TComboBox;
    FAspiration2: TComboBox;
    FAspiration: TComboBox;
    FZodiac: TComboBox;
    FLifeStage: TComboBox;
    FRole: TComboBox;
    FLastName: TEdit;
    FFirstName: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    FNotes: TMemo;
    FLevel: TTrackBar;
    FGeneration: TTrackBar;
  private

  public

  end;

implementation

{$R *.lfm}

end.

