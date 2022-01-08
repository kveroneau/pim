unit dbwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, FileUtil,
  LazFileUtils, simwindow;

type

  { TDBListForm }

  TDBListForm = class(TForm)
    DBList: TListView;
    LargeIcons: TImageList;
    procedure DBListDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    procedure ScanDirectory(const target: string);
  public

  end;

var
  DBListForm: TDBListForm;

implementation

const
  {$IFDEF DEBUG}
  DATA_DIR = 'Data/';
  {$ENDIF}

{$R *.lfm}

{ TDBListForm }

procedure TDBListForm.FormResize(Sender: TObject);
begin
  DBList.Height:=ClientHeight;
  DBList.Width:=ClientWidth;
end;

procedure TDBListForm.ScanDirectory(const target: string);
var
  DBFiles: TStringList;
  i: Integer;
  itm: TListItem;
begin
  if not DirectoryExists(target) then
    Exit;
  DBFiles:=TStringList.Create;
  try
    FindAllFiles(DBFiles, target, '*.sim;*.simdb', False);
    for i:=0 to DBFiles.Count-1 do
    begin
      itm:=DBList.Items.Add;
      itm.Caption:=ExtractFileName(ExtractFileNameWithoutExt(DBFiles.Strings[i]));
      itm.ImageIndex:=0;
    end;
  finally
    DBFiles.Free;
  end;
end;

procedure TDBListForm.FormCreate(Sender: TObject);
begin
  ScanDirectory(DATA_DIR);
end;

procedure TDBListForm.DBListDblClick(Sender: TObject);
var
  FileName: string;
  sim: TSimForm;
  f: TMemoryStream;
begin
  FileName:=DATA_DIR+DBList.ItemFocused.Caption;
  case DBList.ItemFocused.ImageIndex of
    0: FileName:=FileName+'.sim';
  end;
  sim:=TSimForm.Create(Self);
  f:=TMemoryStream.Create;
  try
    f.LoadFromFile(FileName);
    if f.Size > 0 then
      f.ReadComponent(sim.Sim1);
    if sim.ShowModal = mrOK then
    begin
      f.Clear;
      f.WriteComponent(sim.Sim1);
      f.SaveToFile(FileName);
    end;
  finally
    f.Free;
  end;
end;

end.

