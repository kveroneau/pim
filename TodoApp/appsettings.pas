unit appsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    AutoSave: TCheckBox;
    SaveBtn: TButton;
    CloseBtn: TButton;
    DefaultPriority: TEdit;
    ListName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure ResetSettings;
  end;

implementation

{$R *.lfm}

{ TSettingsForm }

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  ResetSettings;
end;

procedure TSettingsForm.ResetSettings;
begin
  ListName.Text:='Untitled';
  DefaultPriority.Text:='50';
  AutoSave.Checked:=False;
end;

end.

