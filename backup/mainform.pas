unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    mnuMainHelpAbout: TMenuItem;
    mnuMainHelp: TMenuItem;
    mnuMainPuppetsExit: TMenuItem;
    Separator2: TMenuItem;
    mnuMainPuppetsSaveAs: TMenuItem;
    Separator1: TMenuItem;
    mnuMainPuppetsOpen: TMenuItem;
    mnuMainPuppetsNew: TMenuItem;
    mnuMainPuppets: TMenuItem;
    mnuMain: TMainMenu;
    procedure mnuMainPuppetsExitClick(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.mnuMainPuppetsExitClick(Sender: TObject);
begin
  Close;
end;

end.

