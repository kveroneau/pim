unit MainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  DBCtrls, StdCtrls, Buttons, Spin, simsdata, DB, simconsts, dbf;

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
    DBBaths: TDBEdit;
    DBBeds: TDBEdit;
    DBCash: TDBEdit;
    DBDays: TDBEdit;
    DBFamilyName: TDBEdit;
    DBFriends: TDBEdit;
    DBFamilyLot: TDBLookupComboBox;
    DBMember1: TDBLookupComboBox;
    DBMember2: TDBLookupComboBox;
    DBMember3: TDBLookupComboBox;
    DBMember4: TDBLookupComboBox;
    DBMember5: TDBLookupComboBox;
    DBMember6: TDBLookupComboBox;
    DBMember7: TDBLookupComboBox;
    DBMember8: TDBLookupComboBox;
    DBLayout: TDBEdit;
    FamilyNavigator: TDBNavigator;
    DBUpkeep: TDBEdit;
    DBYard: TDBEdit;
    DBFurnish: TDBEdit;
    DBStatSize: TDBEdit;
    DBFloors: TDBEdit;
    DBSize: TDBEdit;
    DBValue: TDBEdit;
    DBName: TDBEdit;
    DBJob: TDBEdit;
    FamilyInfoGroup: TGroupBox;
    FamilyLotImage: TImage;
    MemberBtn2: TSpeedButton;
    MemberBtn3: TSpeedButton;
    MemberBtn4: TSpeedButton;
    MemberBtn5: TSpeedButton;
    MemberBtn6: TSpeedButton;
    MemberBtn7: TSpeedButton;
    MemberBtn8: TSpeedButton;
    MembersGroup: TGroupBox;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    LotImage: TImage;
    Label21: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    LotStatisticsGroup: TGroupBox;
    Label22: TLabel;
    Label23: TLabel;
    LotInfoGroup: TGroupBox;
    LotNavigator: TDBNavigator;
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
    SimFace1: TImage;
    SimFace2: TImage;
    SimFace3: TImage;
    SimFace4: TImage;
    SimFace5: TImage;
    SimFace6: TImage;
    SimFace7: TImage;
    SimFace8: TImage;
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
    LotEditor: TTabSheet;
    FamilyEditor: TTabSheet;
    MemberBtn1: TSpeedButton;
    Members: TSpinEdit;
    procedure DBMembersChange(Sender: TObject);
    procedure DBTreeDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrackerSync(Sender: TObject);
    procedure JumpToSim(Sender: TObject);
  private
    procedure GenerateTree(ANode: string; DataSet: TDbf; RecordType: TRecordType);
    procedure ClearRecPointers(AParent: string);
    procedure SetMemebers(count: integer);
  public
    procedure SimsDSChange(Sender: TObject; Field: TField);
    procedure FamilyDSChange(Sender: TObject; Field: TField);
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
  SimsDatabase.FamilyDS.OnDataChange:=@FamilyDSChange;
  GenerateTree('Sims', SimsDatabase.SimDBF, rtSim);
  GenerateTree('Lots', SimsDatabase.LotDBF, rtLot);
  GenerateTree('Families', SimsDatabase.FamilyDBF, rtFamily);
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

procedure TSimsDBForm.JumpToSim(Sender: TObject);
var
  c: TComponent;
begin
  c:=TComponent(Sender);
  SimsDatabase.JumpToSim(SimsDatabase.FamilyDBF.FieldByName('Member'+RightStr(c.Name, 1)).AsInteger);
  EditorTabs.TabIndex:=0;
end;

procedure TSimsDBForm.GenerateTree(ANode: string; DataSet: TDbf;
  RecordType: TRecordType);
var
  node, itm: TTreeNode;
  rec: PRecordPtr;
begin
  node:=DBTree.Items.FindNodeWithText(ANode);
  with DataSet do
  begin
    First;
    repeat
      itm:=DBTree.Items.AddChild(node, FieldByName('Name').AsString);
      itm.ImageIndex:=ord(RecordType);
      itm.SelectedIndex:=ord(RecordType);
      New(rec);
      rec^.table:=RecordType;
      rec^.id:=RecNo;
      itm.Data:=rec;
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

procedure TSimsDBForm.SetMemebers(count: integer);
var
  i: Integer;
  c: TWinControl;
  b: TSpeedButton;
begin
  if (count < 1) or (count > 8) then
    Exit;
  for i:=1 to 8 do
  begin
    c:=TWinControl(FindSubComponent('DBMember'+IntToStr(i)));
    b:=TSpeedButton(FindSubComponent('MemberBtn'+IntToStr(i)));
    if count >= i then
    begin
      c.Visible:=True;
      b.Visible:=True;
    end
    else
    begin
      c.Visible:=False;
      b.Visible:=False;
    end;
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

procedure TSimsDBForm.FamilyDSChange(Sender: TObject; Field: TField);
begin
  Members.Value:=SimsDatabase.FamilyDBF.FieldByName('Members').AsInteger;
end;

procedure TSimsDBForm.FormCreate(Sender: TObject);
begin
  {SimFace.Picture.LoadFromFile('SimsWeb/Families/Golden/family1_face.jpg');
  SimPhoto.Picture.LoadFromFile('SimsWeb/Families/Golden/family1_full.jpg');
  LotImage.Picture.LoadFromFile('SimsWeb/Families/Golden/house-exterior.jpg');}
end;

procedure TSimsDBForm.DBTreeDblClick(Sender: TObject);
var
  rec: PRecordPtr;
begin
  rec:=DBTree.Selected.Data;
  if not Assigned(rec) then
    Exit;
  case rec^.table of
    rtSim: SimsDatabase.SimDBF.RecNo:=rec^.id;
    rtLot: SimsDatabase.LotDBF.RecNo:=rec^.id;
    rtFamily: SimsDatabase.FamilyDBF.RecNo:=rec^.id;
  end;
  EditorTabs.TabIndex:=Ord(rec^.table);
end;

procedure TSimsDBForm.DBMembersChange(Sender: TObject);
begin
  SetMemebers(Members.Value);
  SimsDatabase.FamilyDBF.FieldByName('Members').AsInteger:=Members.Value;
end;

procedure TSimsDBForm.FormDestroy(Sender: TObject);
begin
  ClearRecPointers('Sims');
  ClearRecPointers('Lots');
  ClearRecPointers('Families');
end;

end.

