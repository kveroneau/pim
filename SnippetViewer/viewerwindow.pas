unit ViewerWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TViewerForm }

  TViewerForm = class(TForm)
    SnippetText: TMemo;
    procedure FormResize(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

{ TViewerForm }

procedure TViewerForm.FormResize(Sender: TObject);
begin
  SnippetText.Height:=ClientHeight;
  SnippetText.Width:=ClientWidth;
end;

end.

