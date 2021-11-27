unit playlistedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    NewMenu: TMenuItem;
    OpenDialog: TOpenDialog;
    OpenMenu: TMenuItem;
    SaveDialog: TSaveDialog;
    SaveMenu: TMenuItem;
    ExitMenu: TMenuItem;
    MenuItem6: TMenuItem;
    Playlist: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure NewMenuClick(Sender: TObject);
    procedure OpenMenuClick(Sender: TObject);
    procedure SaveMenuClick(Sender: TObject);
  private
    FFileName: string;
    procedure OpenPlaylist(const fname: string);
    procedure SavePlaylist(const fname: string);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenPlaylist('/home/kveroneau/songs.pls');
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Playlist.Height:=ClientHeight;
end;

procedure TForm1.NewMenuClick(Sender: TObject);
begin
  Playlist.Lines.Clear;
  FFileName:='';
end;

procedure TForm1.OpenMenuClick(Sender: TObject);
begin
  OpenDialog.InitialDir:=GetEnvironmentVariable('HOME');
  if not OpenDialog.Execute then
    Exit;
  Playlist.Lines.Clear;
  OpenPlaylist(OpenDialog.FileName);
end;

procedure TForm1.SaveMenuClick(Sender: TObject);
begin
  if FFileName = '' then
  begin
    SaveDialog.InitialDir:=GetEnvironmentVariable('HOME');
    if not SaveDialog.Execute then
      Exit;
    FFileName:=SaveDialog.FileName;
  end;
  SavePlaylist(FFileName);
end;

procedure TForm1.OpenPlaylist(const fname: string);
var
  f: TMemoryStream;
begin
  FFileName:='';
  f:=TMemoryStream.Create;
  try
    f.LoadFromFile(fname);
    if f.ReadByte = 0 then
    begin
      Playlist.Lines.LoadFromStream(f);
      Playlist.Lines.Delete(0);
      FFileName:=fname;
    end;
  finally
    f.Free;
  end;
end;

procedure TForm1.SavePlaylist(const fname: string);
var
  f: TMemoryStream;
begin
  f:=TMemoryStream.Create;
  try
    f.WriteByte(0);
    Playlist.Lines.Insert(0, 'timidity playlist:');
    Playlist.Lines.SaveToStream(f);
    Playlist.Lines.Delete(0);
    f.SaveToFile(fname);
  finally
    f.Free;
  end;
end;

end.

