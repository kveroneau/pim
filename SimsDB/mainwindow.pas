unit MainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  DBCtrls, StdCtrls, simsdata, DB, simconsts;

type

  TRecordType = (rtSim, rtLot, rtFamily);

  PRecordPtr = ^TRecordPtr;
  TRecordPtr = record
    table: TRecordType;
    id: Integer;
  end;

  { TSimsDBForm }

  TSimsDBForm = class(TForm)
    BasicInfoGroup: TGroupBox;
    Charisma: TTrackBar;
    Body: TTrackBar;
    Creativity: TTrackBar;
    DBCareer: TDBEdit;
    DBJob: TDBEdit;
    Logic: TTrackBar;
    Mechanical: TTrackBar;
    DBSalary: TDBEdit;
    DBPerformance: TDBEdit;
    DBGender: TDBEdit;
    DBLifeStage: TDBLookupComboBox;
    DBZodiac: TDBLookupComboBox;
    DBSimName: TDBEdit;
    GeneratePersonality: TButton;
    Cooking: TTrackBar;
    Outgoing: TTrackBar;
    PActive: TTrackBar;
    Playful: TTrackBar;
    Nice: TTrackBar;
    SimPhoto: TImage;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    SimFace: TImage;
    SkillsGroup: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label9: TLabel;
    PersonalityGroup: TGroupBox;
    JobGroup: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    SimNavigator: TDBNavigator;
    EditorTabs: TPageControl;
    SmallIcons: TImageList;
    SimsLogo: TImage;
    DBTree: TTreeView;
    SimEditor: TTabSheet;
    Neat: TTrackBar;
    procedure DBTreeDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrackerSync(Sender: TObject);
  private
    procedure GenerateSimsTree;
    procedure ClearRecPointers(AParent: string);
  public
    procedure SimsDSChange(Sender: TObject; Field: TField);
  end;

var
  SimsDBForm: TSimsDBForm;

implementation

{$R *.lfm}

{ TSimsDBForm }

procedure TSimsDBForm.FormResize(Sender: TObject);
begin
  DBTree.Height:=ClientHeight-SimsLogo.Height;
  EditorTabs.Height:=DBTree.Height;
  EditorTabs.Width:=ClientWidth-DBTree.Width;
end;

procedure TSimsDBForm.FormShow(Sender: TObject);
begin
  SimsDatabase.SimDS.OnDataChange:=@SimsDSChange;
  GenerateSimsTree;
end;

procedure TSimsDBForm.TrackerSync(Sender: TObject);
var
  t: TTrackBar;
begin
  t:=TTrackBar(Sender);
  if t.Name = 'PActive' then
    SimsDatabase.SimDBF.FieldByName('Active').AsInteger:=t.Position
  else
    SimsDatabase.SimDBF.FieldByName(t.Name).AsInteger:=t.Position;
end;

procedure TSimsDBForm.GenerateSimsTree;
var
  sims, sim: TTreeNode;
  rec: PRecordPtr;
begin
  sims:=DBTree.Items.FindNodeWithText('Sims');
  with SimsDatabase.SimDBF do
  begin
    First;
    repeat
      sim:=DBTree.Items.AddChild(sims, FieldByName('Name').AsString);
      sim.ImageIndex:=0;
      sim.SelectedIndex:=0;
      New(rec);
      rec^.table:=rtSim;
      rec^.id:=RecNo;
      sim.Data:=rec;
      Next;
    until EOF;
  end;
end;

procedure TSimsDBForm.ClearRecPointers(AParent: string);
var
  node: TTreeNode;
  rec: PRecordPtr;
  i: Integer;
begin
  node:=DBTree.Items.FindNodeWithText(AParent);
  for i:=0 to node.Count-1 do
  begin
    rec:=node.Items[i].Data;
    Dispose(rec);
  end;
end;

procedure TSimsDBForm.SimsDSChange(Sender: TObject; Field: TField);
var
  i: Integer;
  t: TTrackBar;
begin
  for i:=0 to Length(SIM_PERSONALITY)-1 do
  begin
    if SIM_PERSONALITY[i] = 'Active' then
      t:=FindSubComponent('P'+SIM_PERSONALITY[i]) as TTrackBar
    else
      t:=FindSubComponent(SIM_PERSONALITY[i]) as TTrackBar;
    t.Position:=SimsDatabase.SimDBF.FieldByName(SIM_PERSONALITY[i]).AsInteger;
  end;
  for i:=0 to Length(SIM_SKILLS)-1 do
  begin
    t:=FindSubComponent(SIM_SKILLS[i]) as TTrackBar;
    t.Position:=SimsDatabase.SimDBF.FieldByName(SIM_SKILLS[i]).AsInteger;
  end;
end;

procedure TSimsDBForm.FormCreate(Sender: TObject);
begin
  {SimFace.Picture.LoadFromFile('SimsWeb/Families/Golden/family1_face.jpg');
  SimPhoto.Picture.LoadFromFile('SimsWeb/Families/Golden/family1_full.jpg');}
end;

procedure TSimsDBForm.DBTreeDblClick(Sender: TObject);
var
  rec: PRecordPtr;
begin
  rec:=DBTree.Selected.Data;
  case rec^.table of
    rtSim: SimsDatabase.SimDBF.RecNo:=rec^.id;
  end;
end;

procedure TSimsDBForm.FormDestroy(Sender: TObject);
begin
  ClearRecPointers('Sims');
end;

end.

