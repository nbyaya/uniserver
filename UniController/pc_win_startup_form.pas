unit pc_win_startup_form;

{#############################################################################
'# Name: pc_win_startup_form.pas
'# Developed By: The Uniform Server Development Team
'# Web: https://www.uniformserver.com
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  StdCtrls, default_config_vars,
  us_common_procedures,
  us_common_functions,
  registry,
  Windows;

Const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5)); //Is admin

type

  { Tpc_win_startup }

  Tpc_win_startup = class(TForm)
    CB_enable_run_apache: TCheckBox;
    CB_enable_run_mysql: TCheckBox;
    CB_enable_run: TCheckBox;
    DividerBevel1: TDividerBevel;
    GB_pc_win_startup: TGroupBox;
    GB_enable_disable: TGroupBox;
    procedure CB_enable_runChange(Sender: TObject);
    procedure CB_enable_run_apacheChange(Sender: TObject);
    procedure CB_enable_run_mysqlChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  pc_win_startup: Tpc_win_startup;

implementation

uses
main_unit;

{$R *.lfm}

{===================================================================
The CheckTokenMembership function determines whether a specified
 security identifier (SID) is enabled in an access token.
 Uses: Windows
====================================================================}
function CheckTokenMembership(TokenHandle: THandle; SidToCheck: PSID; var IsMember: BOOL): BOOL; stdcall; external advapi32;
{---End CheckTokenMembership ---------------------------------------}

{===================================================================
 This function tells us if this application is running with
 administrative privelages.

 In Vista and later, if you are non-elevated, this function will
 return false (not running with administrative privelages).

 Otherwise returns true, running with administrative privelages.

 Uses: Windows
====================================================================}
function IsUserAdmin: Boolean;
var
    b: BOOL;
    AdministratorsGroup: PSID;
begin
    b := AllocateAndInitializeSid(
            SECURITY_NT_AUTHORITY,
            2, //2 sub-authorities
            SECURITY_BUILTIN_DOMAIN_RID,    //sub-authority 0
            DOMAIN_ALIAS_RID_ADMINS,        //sub-authority 1
            0, 0, 0, 0, 0, 0,               //sub-authorities 2-7 not passed
            AdministratorsGroup);
    if (b) then
    begin
        if not CheckTokenMembership(0, AdministratorsGroup, b) then
            b := False;
        FreeSid(AdministratorsGroup);
    end;

    Result := b;
end;
{---End IsUserAdmin ------------------------------------------------}



{===================================================================
Run on Windows start:

Uses: registry
====================================================================}
procedure RunOnWinStart(ApTitle, ApPathFile: string; RunOnce: Boolean);
var
  Reg: TRegistry;
  TheKey: string;
begin
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  TheKey := 'Software\Microsoft\Windows\CurrentVersion\Run';
  if RunOnce then TheKey := TheKey + 'Once';
  // Open key, or create it if it doesn't exist
  Reg.OpenKey(TheKey, True);
  Reg.WriteString(ApTitle, ApPathFile);
  Reg.CloseKey;
  Reg.Free;
end;
{---End RunOnWinStart ----------------------------------------------}

{===================================================================
Remove from run key:

Uses: registry
====================================================================}
procedure RemoveFromRunKey(ApTitle: string);
var
  Reg           :TRegistry;
  TheKey        :string;
begin
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  TheKey := 'Software\Microsoft\Windows\CurrentVersion\Run';
  // Check if key exist...
  // ...if yes, try to delete the entry for ApTitle
  if not Reg.OpenKey(TheKey, False) then
    us_MessageDlg('注册表信息','未找到注册表项', mtcustom,[mbOk],0) //Display information message
  else begin
    if Reg.DeleteValue(ApTitle) then
     us_MessageDlg('注册表信息','已从注册表中删除条目', mtcustom,[mbOk],0) ; //Display information message
//    ShowMessage('Removed: ' + ApTitle);
//    else
//      ShowMessage('Not found: ' + ApTitle);
  end;
  Reg.CloseKey;
  Reg.Free;
end;
{--- End RemoveFromRunKey ------------------------------------------}


{===================================================================
PC-Win start-up selection:
 Validation and config update.
 Windows registry update.

 View setting cmd msconfig
====================================================================}
procedure pc_win_startup_validation;
var
  str :String;
begin
 USC_RunApacheAtStartUp    := pc_win_startup.CB_enable_run_apache.Checked; // Get new selection
 USC_RunMysqlAtStartUp     := pc_win_startup.CB_enable_run_mysql.Checked;  // Get new selection
 USC_RunAtPcStartUpEnabled := pc_win_startup.CB_enable_run.Checked;        // Get new selection

 If Not(USC_RunApacheAtStartUp) and Not(USC_RunMysqlAtStartUp) and USC_RunAtPcStartUpEnabled Then
   begin
     USC_RunAtPcStartUpEnabled := False;            //Set to false
     pc_win_startup.CB_enable_run.Checked := False; //UnCheck
     us_MessageDlg('错误','注意：至少必须选择一个项目！' + sLineBreak +' 请选择一个要运行的项目', mtError,[mbOk],0) ; //Display information message
   end;

 //Save new values to config file
 If USC_RunApacheAtStartUp Then str:='true' Else str:='false'; //Convert bool to text
 us_ini_set(USF_US_CONF_INI,'PCSTARTUP','RunApacheAtStartUp',    str);

 If USC_RunMysqlAtStartUp Then str:='true' Else str:='false';
 us_ini_set(USF_US_CONF_INI,'PCSTARTUP','RunMysqlAtStartUp',     str);

 If USC_RunAtPcStartUpEnabled Then str:='true' Else str:='false';
 us_ini_set(USF_US_CONF_INI,'PCSTARTUP','RunAtPcStartUpEnabled', str);

 //Set Windows registry to reflect USC_RunAtPcStartUpEnabled
 If USC_RunAtPcStartUpEnabled Then
   begin
    RunOnWinStart('UniServerRun', UniConPath+'\UniController.exe pc_win_start', False) //Set registry
    //showmessage('Set');
   end
 Else
   begin
     RemoveFromRunKey('UniServerRun');   //Remove from registry
     // showmessage('remove');
   end;

end;
{---End pc_win_startup_validation ----------------------------------}


{ Tpc_win_startup }

procedure Tpc_win_startup.CB_enable_runChange(Sender: TObject);
begin
  //Run check box - Check state-change using xor
  If USC_RunAtPcStartUpEnabled xor CB_enable_run.Checked Then pc_win_startup_validation;
end;

procedure Tpc_win_startup.CB_enable_run_apacheChange(Sender: TObject);
begin
 //Apache check box - Check state-change using xor
 If USC_RunApacheAtStartUp xor CB_enable_run_apache.Checked Then  pc_win_startup_validation;
end;

procedure Tpc_win_startup.CB_enable_run_mysqlChange(Sender: TObject);
begin
 //MySQL check box  - Check state-change using xor
 If USC_RunMysqlAtStartUp xor CB_enable_run_mysql.Checked Then  pc_win_startup_validation;
end;

procedure Tpc_win_startup.FormCreate(Sender: TObject);
begin
 // 汉化界面
 Caption := '启动项设置';
 GB_pc_win_startup.Caption := '选择启动时运行的项目';
 CB_enable_run_apache.Caption := 'Apache 服务器';
 CB_enable_run_mysql.Caption := 'MySQL 服务器';
 GB_enable_disable.Caption := '启用/禁用启动时运行';
 CB_enable_run.Caption := '启动时运行';
 
 //==If Apache or MySQL installed enable pc_win_startup menu button
 If (DirectoryExists(US_APACHE)) or (DirectoryExists(US_MYSQL)) Then
    begin
      Main.MMS_pc_win_startup.Enabled   := true;   // Enable menu button
     end
 Else
  begin
    Main.MMS_pc_win_startup.Enabled   := false;    // Disable menu button
    // Note user may have changed ini-file hence force update with real state)
    USC_RunAtPcStartUpEnabled := False;                                      // Set var to false.
    us_ini_set(USF_US_CONF_INI,'PCSTARTUP','RunAtPcStartUpEnabled','false'); // Save to config file
  end;

 //==Enable Apache check box
 //  Check Apache Installed (Note user may have changed ini-file hence force update with real state)
 If DirectoryExists(US_APACHE) Then CB_enable_run_apache.Enabled := true     // Enable check box
   else                                                                      // Apache not installed
     begin
       CB_enable_run_apache.Enabled:=False;                                  // Disable check box
       USC_RunApacheAtStartUp := False;                                      // Set var to false.
       us_ini_set(USF_US_CONF_INI,'PCSTARTUP','RunApacheAtStartUp','false'); // Save to config file
     end;

 //==Enable MySQL check box
 //  Check MySQL Installed (Note user may have changed ini-file hence force update with real state)
 If DirectoryExists(US_MYSQL) Then CB_enable_run_mysql.Enabled := true       // Enable check box
  else                                                                       // MySQL not installed
    begin
      CB_enable_run_mysql.Enabled:=False;                                    // Disable check box
      USC_RunMysqlAtStartUp := False;                                        // Set var to false.
      us_ini_set(USF_US_CONF_INI,'PCSTARTUP','RunMysqlAtStartUp','false');   // Save to config file
    end;

end;

procedure Tpc_win_startup.FormShow(Sender: TObject);
var
  str:string;
  str_title: string;
begin
 //==Set Apache check box
 If USC_RunApacheAtStartUp Then CB_enable_run_apache.Checked:=True
   else CB_enable_run_apache.Checked:=False;

 //==Set MySQL check box
 If USC_RunMysqlAtStartUp Then CB_enable_run_mysql.Checked:=True
   else CB_enable_run_mysql.Checked:=False;

 //==Set RUN check box
 If USC_RunAtPcStartUpEnabled Then CB_enable_run.Checked:=True
   else CB_enable_run.Checked:=False;

 //==Check running with administrative privelages
 If  IsUserAdmin Then
  begin // Running with administrative privelages
   GB_pc_win_startup.Enabled := True; //Enable check boxes
   GB_enable_disable.Enabled := True; //Enable check boxes
  end
 Else
  begin // NOT running with administrative privelages
   GB_pc_win_startup.Enabled := False; //Disable check boxes
   GB_enable_disable.Enabled := False; //Disable check boxes
   //Inform user what to do
   str_title := '需要管理员权限'; //Title
   str :='';

   str := str + '此菜单项被禁用，因为它需要管理员权限。'         + sLineBreak;
   str := str + '要以管理员身份运行菜单项，请按以下步骤操作：'         + sLineBreak + sLineBreak;

   str := str + '1) 关闭此菜单。'         + sLineBreak;
   str := str + '2) 如果正在运行，请停止 Apache 和 MySQL 服务器。'         + sLineBreak;
   str := str + '3) 关闭 UniController'         + sLineBreak + sLineBreak;

   str := str + '4) 右键单击 UniController.exe'         + sLineBreak;
   str := str + '5) 选择"以管理员身份运行"'         + sLineBreak;
   str := str + '6) 此菜单项将被启用。'         + sLineBreak + sLineBreak;

   str := str + '注意：'         + sLineBreak;
   str := str + '选择所需的菜单选项后，请关闭菜单和 UniController。'         + sLineBreak;
   str := str + '通过双击 UniController.exe 重新启动 UniController'         + sLineBreak;
   str := str + '这可以防止 UniController 以管理员身份运行。永久以管理员身份运行'         + sLineBreak;
   str := str + '存在安全风险。'         + sLineBreak;

   us_MessageDlg(str_title, str, mtInformation,[mbOk],0) ; //Display message
  end;

end;

end.

