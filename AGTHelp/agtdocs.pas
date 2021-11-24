unit agtdocs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { THelpForm }

  THelpForm = class(TForm)
    SectionList: TListBox;
    TextViewer: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure SectionListClick(Sender: TObject);
  private

  public

  end;

var
  HelpForm: THelpForm;

implementation

const
  AGT_DOC = '/DOS/IF/AGT17/AGT-DOC.TXT';
  AGT_SECTIONS: Array[0..13] of integer = (
           84190,
           90227,
           104255,
           111252,
           127552,
           128929,
           142967,
           144008,
           149344,
           160129,
           169022,
           179969,
           184311,
           196716);

{$R *.lfm}

{ THelpForm }

procedure THelpForm.FormCreate(Sender: TObject);
begin
  TextViewer.Lines.LoadFromFile(AGT_DOC);
end;

procedure THelpForm.SectionListClick(Sender: TObject);
begin
  TextViewer.SelStart:=AGT_SECTIONS[SectionList.ItemIndex];
end;

end.

