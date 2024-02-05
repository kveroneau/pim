unit PushPullWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  RTTIGrids, RTTICtrls, LazMemcard, LazNetcard, memcard;

type

  TPushPullMode = (ppmPush, ppmPull);

  { TPushPullForm }

  TPushPullForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LazNetcard: TLazNetcard;
    BlockList: TListBox;
    TICheckBox1: TTICheckBox;
    TIEdit1: TTIEdit;
    TIEdit2: TTIEdit;
    TIEdit3: TTIEdit;
    TIEdit4: TTIEdit;
    procedure BlockListDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormHide(Sender: TObject);
    procedure TICheckBox1Change(Sender: TObject);
  private
    FBlock: TMemoryStream;
    FInfo: PBlockInfo;
    FMode: TPushPullMode;
    procedure PopulateList;
  public
    procedure PushSnippet(blk: TMemoryStream; info: PBlockInfo);
    function PullSnippet(blk: TMemoryStream; info: PBlockInfo): TModalResult;
  end;

var
  PushPullForm: TPushPullForm;

implementation

{$R *.lfm}

{ TPushPullForm }

procedure TPushPullForm.TICheckBox1Change(Sender: TObject);
begin
  if LazNetcard.Active then
    PopulateList
  else
    BlockList.Clear;
end;

procedure TPushPullForm.PopulateList;
var
  lst: TStringList;
begin
  BlockList.Clear;
  lst:=LazNetcard.BlockList;
  BlockList.Items.AddStrings(lst);
  lst.Free;
end;

procedure TPushPullForm.FormHide(Sender: TObject);
begin
  if LazNetcard.Active then
    LazNetcard.Active:=False;
end;

procedure TPushPullForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CloseAction:=caHide;
  if LazNetcard.Active then
    LazNetcard.Active:=False;
end;

procedure TPushPullForm.BlockListDblClick(Sender: TObject);
begin
  WriteLn(BlockList.ItemIndex);
  LazNetcard.BlockID:=BlockList.ItemIndex+1;
  if not LazNetcard.Active then
    ShowMessage('Uh Oh.');
  if not Assigned(LazNetcard.Block) then
    Exit;
  LazNetcard.Block.Position:=0;
  if FMode = ppmPush then
  begin
    LazNetcard.Block.LoadFromStream(FBlock);
    Move(FInfo^, LazNetcard.BlockInfo^, SizeOf(FInfo^));
    LazNetcard.Write;
    PopulateList;
  end
  else if FMode = ppmPull then
  begin
    FBlock.Position:=0;
    LazNetcard.Block.SaveToStream(FBlock);
    Move(LazNetcard.BlockInfo^, FInfo^, SizeOf(FInfo^));
    ModalResult:=mrOK;
    Close;
  end;
end;

procedure TPushPullForm.PushSnippet(blk: TMemoryStream; info: PBlockInfo);
begin
  FBlock:=blk;
  FInfo:=info;
  Caption:='Push Snippet to Server...';
  FMode:=ppmPush;
  ShowModal;
end;

function TPushPullForm.PullSnippet(blk: TMemoryStream; info: PBlockInfo): TModalResult;
begin
  FBlock:=blk;
  FInfo:=info;
  Caption:='Pull Snippet from Server...';
  FMode:=ppmPull;
  Result:=ShowModal;
end;

end.

