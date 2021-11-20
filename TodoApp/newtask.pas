unit newtask;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TNewTaskForm }

  TNewTaskForm = class(TForm)
    CancelBtn: TButton;
    OkayBtn: TButton;
    TaskType: TComboBox;
    TaskTitle: TEdit;
    TaskPoints: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private

  public

  end;

var
  NewTaskForm: TNewTaskForm;

implementation

{$R *.lfm}

end.

