unit SnippetWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ComCtrls, ExtCtrls, memcard, BlowFish, PushPullWindow, ViewerWindow;

type

  { TSnippetForm }

  TSnippetForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    MenuItem1: TMenuItem;
    DelMnu: TMenuItem;
    MenuItem2: TMenuItem;
    ExitMnu: TMenuItem;
    HelpMnu: TMenuItem;
    AboutMnu: TMenuItem;
    EditMenu: TMenuItem;
    LrgFontMenu: TMenuItem;
    MenuItem3: TMenuItem;
    MountMenu: TMenuItem;
    PushMenu: TMenuItem;
    PullMenu: TMenuItem;
    NetworkMnu: TMenuItem;
    SmFontMenu: TMenuItem;
    PassKeyMnu: TMenuItem;
    SaveMnu: TMenuItem;
    NewMnu: TMenuItem;
    CategoryList: TComboBox;
    NoteTitle: TEdit;
    Label1: TLabel;
    NoteEdit: TMemo;
    NoteList: TListBox;
    StatusBar: TStatusBar;
    Timer: TTimer;
    procedure AboutMnuClick(Sender: TObject);
    procedure CategoryListChange(Sender: TObject);
    procedure DelMnuClick(Sender: TObject);
    procedure ExitMnuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure LrgFontMenuClick(Sender: TObject);
    procedure NewBtnClick(Sender: TObject);
    procedure NoteEditKeyPress(Sender: TObject; var Key: char);
    procedure NoteListClick(Sender: TObject);
    procedure NoteListDblClick(Sender: TObject);
    procedure PassKeyMnuClick(Sender: TObject);
    procedure PullMenuClick(Sender: TObject);
    procedure PushMenuClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure SmFontMenuClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FCard: TMemCard;
    FBlockID: byte;
    FBlock: TMemoryStream;
    FInfo: PBlockInfo;
    FPassKey: string;
    procedure SetCategory(const cna: string);
    procedure SetStatus(const s: string; SetTimer: Boolean);
  public

  end;

var
  SnippetForm: TSnippetForm;

implementation

const
  VERSION = '0.5.1b';
  NULL_KEY = 'TestKey123';

{$R *.lfm}

{ TSnippetForm }

procedure TSnippetForm.FormCreate(Sender: TObject);
begin
  Caption:=Application.Title;
  FCard:=Nil;
  FBlock:=Nil;
  FInfo:=Nil;
  FPassKey:=NULL_KEY;
  CategoryList.ItemIndex:=0;
  SetCategory(CategoryList.Caption);
end;

procedure TSnippetForm.CategoryListChange(Sender: TObject);
begin
  SetCategory(CategoryList.Caption);
end;

procedure TSnippetForm.AboutMnuClick(Sender: TObject);
begin
  ShowMessage('Snippet Viewer v'+VERSION);
end;

procedure TSnippetForm.DelMnuClick(Sender: TObject);
begin
  FCard.DeleteBlock(FBlockID);
  FBlockID:=0;
  FreeAndNil(FBlock);
  SetCategory(CategoryList.Caption);
  SetStatus('Note Deleted.', True);
end;

procedure TSnippetForm.ExitMnuClick(Sender: TObject);
begin
  Close;
end;

procedure TSnippetForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FCard) then
    FCard.Free;
  if Assigned(FBlock) then
    FBlock.Free;
  if Assigned(FInfo) then
    Dispose(FInfo);
end;

procedure TSnippetForm.FormResize(Sender: TObject);
begin
  NoteList.Height:=ClientHeight-CategoryList.Height-StatusBar.Height-2;
  CategoryList.Top:=NoteList.Height+3;
  NoteEdit.Height:=ClientHeight-NoteTitle.Height-StatusBar.Height-2;
  NoteEdit.Width:=ClientWidth-NoteList.Width;
end;

procedure TSnippetForm.LrgFontMenuClick(Sender: TObject);
begin
  NoteEdit.Font.Size:=16;
end;

procedure TSnippetForm.NewBtnClick(Sender: TObject);
var
  blkid: byte;
begin
  blkid:=FCard.FindFree;
  if blkid = 0 then
  begin
    SetStatus('There is no more space left in this category!', False);
    Exit;
  end;
  FBlockID:=blkid;
  if Assigned(FBlock) then
    FBlock.Free;
  FBlock:=FCard.ReadBlock(FBlockID);
  NoteTitle.Text:='Untitled';
  NoteEdit.Text:='';
  SetStatus('New Note Ready.', True);
end;

procedure TSnippetForm.NoteEditKeyPress(Sender: TObject; var Key: char);
var
  line: string;
begin
  if Key = #13 then
  begin
    line:=NoteEdit.Lines.Strings[NoteEdit.Lines.Count-1];
    if line = 'dt' then
      NoteEdit.Lines.Strings[NoteEdit.Lines.Count-1]:=FormatDateTime('dddd mmmm d, yyyy "at" hh:nn', Now);
  end;
  SetStatus(IntToStr(Length(NoteEdit.Text))+'/'+IntToStr(FCard.BlockSize)+' characters.', False);
end;

procedure TSnippetForm.NoteListClick(Sender: TObject);
begin
  SetStatus('Loading Note...', False);
  FBlockID:=NoteList.ItemIndex+1;
  if Assigned(FBlock) then
    FBlock.Free;
  if not Assigned(FInfo) then
    New(FInfo);
  FCard.GetInfo(FBlockID, FInfo);
  NoteTitle.Caption:=FInfo^.title;
  FBlock:=FCard.ReadBlock(FBlockID);
  if FInfo^.typno = 8 then
    NoteEdit.Text:=FBlock.ReadAnsiString
  else if FPassKey = NULL_KEY then
    NoteEdit.Text:='PassKey Not Set.'
  else if FInfo^.typno = $bf then
  begin
    with TBlowFishDeCryptStream.Create(FPassKey, FBlock) do
    begin
      try
        NoteEdit.Text:=ReadAnsiString;
      finally
        Free;
      end;
    end;
  end;
  SetStatus('Note Loaded.', True);
end;

procedure TSnippetForm.NoteListDblClick(Sender: TObject);
var
  f: TViewerForm;
begin
  f:=TViewerForm.Create(Self);
  f.Caption:=NoteTitle.Text;
  f.SnippetText.Text:=NoteEdit.Text;
  f.Show;
end;

procedure TSnippetForm.PassKeyMnuClick(Sender: TObject);
var
  s: string;
begin
  s:='';
  if InputQuery(Application.Title, 'Specify PassKey:', True, s) then
  begin
    FPassKey:=s;
    PassKeyMnu.Enabled:=False;
    SetStatus('PassKey Set.', True);
  end;
end;

procedure TSnippetForm.PullMenuClick(Sender: TObject);
begin
  if not Assigned(FBlock) then
    FBlock:=FCard.ReadBlock(FBlockID);
  if PushPullForm.PullSnippet(FBlock, FInfo) = mrOK then
    Exit;
  WriteLn('Does the result come back right?');
  NoteTitle.Caption:=FInfo^.title;
  FBlock.Position:=0;
  if FInfo^.typno = 8 then
    NoteEdit.Text:=FBlock.ReadAnsiString
  else if FPassKey = NULL_KEY then
    NoteEdit.Text:='PassKey Not Set.'
  else if FInfo^.typno = $bf then
  begin
    with TBlowFishDeCryptStream.Create(FPassKey, FBlock) do
    begin
      try
        NoteEdit.Text:=ReadAnsiString;
      finally
        Free;
      end;
    end;
  end;
  SetStatus('Note Pulled.', True);
end;

procedure TSnippetForm.PushMenuClick(Sender: TObject);
begin
  PushPullForm.PushSnippet(FBlock, FInfo);
end;

procedure TSnippetForm.SaveBtnClick(Sender: TObject);
begin
  if not Assigned(FBlock) then
  begin
    SetStatus('No note selected.', True);
    Exit;
  end;
  if not Assigned(FInfo) then
  begin
    SetStatus('Allocating Memory...', False);
    New(FInfo);
    with FInfo^ do
    begin
      appno:=9;
      nextid:=0;
    end;
  end;
  FInfo^.title:=NoteTitle.Text;
  if FPassKey = NULL_KEY then
    FInfo^.typno:=8
  else
    FInfo^.typno:=$bf;
  FInfo^.total:=Length(NoteEdit.Text);
  FBlock.Position:=0;
  if FPassKey = NULL_KEY then
    FBlock.WriteAnsiString(NoteEdit.Text)
  else
    with TBlowFishEncryptStream.Create(FPassKey, FBlock) do
      try
        WriteAnsiString(NoteEdit.Text);
      finally
        Free;
      end;
  FCard.WriteBlock(FBlockID, FBlock, FInfo);
  SetCategory(CategoryList.Caption);
  NoteList.ItemIndex:=FBlockID-1;
  SetStatus('Note Saved.', True);
end;

procedure TSnippetForm.SmFontMenuClick(Sender: TObject);
begin
  NoteEdit.Font.Size:=10;
end;

procedure TSnippetForm.TimerTimer(Sender: TObject);
begin
  SetStatus('Ready', False);
  Timer.Enabled:=False;
end;

procedure TSnippetForm.SetCategory(const cna: string);
var
  i: integer;
  lst: TStringList;
begin
  SetStatus('Setting Category...', False);
  if Assigned(FCard) then
    FCard.Free;
  FCard:=TMemCard.Create(cna+'.kmc', 4096);
  NoteList.Clear;
  lst:=FCard.BlockList;
  try
    for i:=0 to lst.Count-1 do
      NoteList.Items.Add(lst.Strings[i]);
  finally
    lst.Free;
  end;
  SetStatus('Category set.', True);
end;

procedure TSnippetForm.SetStatus(const s: string; SetTimer: Boolean);
begin
  StatusBar.SimpleText:=s;
  if SetTimer then
    Timer.Enabled:=True;
  Application.ProcessMessages;
end;

end.

