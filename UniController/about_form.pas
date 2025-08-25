unit about_form;

{#############################################################################
'# Name: about_form.pas
'# Developed By: The Uniform Server Development Team
'# Web: https://www.uniformserver.com
'#############################################################################}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, default_config_vars;

type

  { TAbout }

  TAbout = class(TForm)
    Bevel1: TBevel;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  About: TAbout;

implementation

{$R *.lfm}

{ TAbout }

procedure TAbout.FormCreate(Sender: TObject);
var
  str1:string;
  str2:string;
begin
   // 汉化界面
   about.Caption := '关于';

  str1:='';
  str1:= str1 + 'UniServer Zero XV '+ USC_AppVersion +sLineBreak;
  str1:= str1 + 'UniController XV ' + UNICONTROLLER_VERSION;
  Label1.Caption := str1;

  str2:='';
  str2:= str2 + 'UniController 使用 Pascal 编写并用 Lazarus 编译' +sLineBreak+sLineBreak;
  str2 := str2 + '为 Uniform Server 做出贡献的人员/开发者：' + sLineBreak;
  str2 := str2 + '- Olajide Olaolorun (olajideolaolorun)' +sLineBreak;
  str2 := str2 + '- Mike Gleaves (Ric)' +sLineBreak;
  str2 := str2 + '- Bob Strand (BobS)'+sLineBreak;
  str2 := str2 + '- Sudeep DSouza (SudeepJD)'+sLineBreak;
  str2 := str2 + '- Davide Bonsangue (BrainStorm)'+sLineBreak;
  str2 := str2 + '- Sylvain Bourdon (sbourdon)'+sLineBreak;
  str2 := str2 + '- 汉化：鸦鸦'+sLineBreak+sLineBreak;
  Label2.Caption := str2;
end;

end.

