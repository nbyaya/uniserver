unit root_www_pass_access_form;

{#############################################################################
'# Name: root_www_pass_access_form.pas
'# Developed By: The Uniform Server Development Team
'# Web: https://www.uniformserver.com
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  default_config_vars,
  us_common_functions;

type

  { Troot_www_pass_access }

  Troot_www_pass_access = class(TForm)
    Btn_add_to_list: TButton;
    Btn_delete_selected: TButton;
    Edit_name: TEdit;
    Edit_password: TEdit;
    GBox_enter_name_password: TGroupBox;
    GBox_enable_disable_password: TGroupBox;
    GBox_access: TGroupBox;
    Label_name: TLabel;
    Label_password: TLabel;
    ListBox1: TListBox;
    RBtn_password_disabled: TRadioButton;
    RBtn_password_enabled: TRadioButton;
    RBtn_local: TRadioButton;
    RBtn_local_intranet: TRadioButton;
    RBtn_local_intranet_internet: TRadioButton;
    procedure Btn_add_to_listClick(Sender: TObject);
    procedure Btn_delete_selectedClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RBtn_localClick(Sender: TObject);
    procedure RBtn_local_intranetClick(Sender: TObject);
    procedure RBtn_local_intranet_internetClick(Sender: TObject);
    procedure RBtn_password_disabledClick(Sender: TObject);
    procedure RBtn_password_enabledClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  root_www_pass_access: Troot_www_pass_access;

implementation

{$R *.lfm}

{====================================================================
www Access Initiliasion:
 Sets button and caption text.
 Display name-password pairs set in config file.
 Display password and access state set in config file.

A radio button defines which section is executed.
 These radio buttons initially have their OnClick event disabled.
 This prevents generating an event on setting initial button state.
 After setting initial state OnClick events are enabled.
=====================================================================}
procedure www_access_init;
var
 sList: TStringList;   // String list
 i:integer;
begin
  //===Set initial button state from configuration file settings

   //--- Display name-password pairs containd in config file
   if FileExists(USF_WWW_PASSWORD) Then
    begin
       root_www_pass_access.ListBox1.Items.Clear;    // Clear list
       sList := TStringList.Create;                  // Create object
       sList.LoadFromFile(USF_WWW_PASSWORD);         // Load file
       root_www_pass_access.ListBox1.Items := sList; // Add list to
       sList.Free;                                   // Remove from memory
    end;

   //--- Display password and access state from config file www\.htaccess
   if FileExists(USF_WWW_HTACCESS) Then
    begin
       sList := TStringList.Create;             // Create object
       sList.LoadFromFile(USF_WWW_HTACCESS);    // Load file

       //Disable radio button OnClick events
       root_www_pass_access.RBtn_password_disabled.OnClick       := nil; // Disable event
       root_www_pass_access.RBtn_password_enabled.OnClick        := nil;
       root_www_pass_access.RBtn_local.OnClick                   := nil;
       root_www_pass_access.RBtn_local_intranet.OnClick          := nil;
       root_www_pass_access.RBtn_local_intranet_internet.OnClick := nil;


       //Set button initial state
       root_www_pass_access.RBtn_password_disabled.Checked       := False;
       root_www_pass_access.RBtn_password_enabled.Checked        := False;
       root_www_pass_access.RBtn_local.Checked                   := False;
       root_www_pass_access.RBtn_local_intranet.Checked          := False;
       root_www_pass_access.RBtn_local_intranet_internet.Checked := False;

       //Scan list
       for i:=0 to sList.Count-1 do
        begin
         // Display password enabled state
         If Pos('#AuthName',sList[i]) = 1 then root_www_pass_access.RBtn_password_disabled.Checked := True;
         If Pos('AuthName',sList[i])  = 1 then root_www_pass_access.RBtn_password_enabled.Checked  := True;

         // Display access state
         If Pos('Allow from 127.0.0.1',sList[i])          = 1 then root_www_pass_access.RBtn_local.Checked := True;                   //Local access
         If Pos('Allow from 127.0.0.1 192.168',sList[i])  = 1 then root_www_pass_access.RBtn_local_intranet.Checked := True;          //local and Intranet access
         If Pos('#Allow from 127.0.0.1',sList[i])         = 1 then root_www_pass_access.RBtn_local_intranet_internet.Checked := True; //Internet access enabled
        end;

       //Enable radio button OnClick events
       root_www_pass_access.RBtn_password_disabled.OnClick       := @root_www_pass_access.RBtn_password_disabledClick;
       root_www_pass_access.RBtn_password_enabled.OnClick        := @root_www_pass_access.RBtn_password_enabledClick;
       root_www_pass_access.RBtn_local.OnClick                   := @root_www_pass_access.RBtn_localClick;
       root_www_pass_access.RBtn_local_intranet.OnClick          := @root_www_pass_access.RBtn_local_intranetClick;
       root_www_pass_access.RBtn_local_intranet_internet.OnClick := @root_www_pass_access.RBtn_local_intranet_internetClick;

      sList.Free; // Remove from memory
    end;
 end;
{---End www_access_init --------------------------------------------}


{====================================================================
This procedure enables or  disables password access section in
configuration file www\.htaccess

A radio button defines which section is executed.
  Password Disabled
  Password Enabled
=====================================================================}
procedure www_enable_disable_password_section;
var
    sList: TStringList;   // String list
    i:integer;            // Loop counter
begin
   // Load configuration file .htaccess
   if FileExists(USF_WWW_HTACCESS) Then
    begin
       sList := TStringList.Create;             // Create object
       sList.LoadFromFile(USF_WWW_HTACCESS);    // Load file

       // Disable block
       If root_www_pass_access.RBtn_password_disabled.Checked Then
        begin
           //Scan list
           for i:=0 to sList.Count-1 do
            begin
               If Pos('AuthName',sList[i]) = 1 then
                 sList[i] := '#AuthName "Uniform Server - Server Access"';

               If Pos('AuthType Basic',sList[i]) = 1 then
                 sList[i] := '#AuthType Basic';

               If Pos('AuthUserFile',sList[i]) = 1 then
                 sList[i] := '#AuthUserFile ../../htpasswd/www/.htpasswd';

               If Pos('Require',sList[i]) = 1 then
                 sList[i] := '#Require valid-user';
            end;
        end;

       // Enable block
       If root_www_pass_access.RBtn_password_enabled.Checked Then
        begin
           //Scan list
           for i:=0 to sList.Count-1 do
            begin
               If Pos('#AuthName',sList[i]) = 1 then
                  sList[i] := 'AuthName "Uniform Server - Server Access"';

               If Pos('#AuthType Basic',sList[i]) = 1 then
                  sList[i] := 'AuthType Basic';

               If Pos('#AuthUserFile',sList[i]) = 1 then
                  sList[i] := 'AuthUserFile ../../htpasswd/www/.htpasswd';

               If Pos('#Require',sList[i]) = 1 then
                  sList[i] := 'Require valid-user';
             end;
           //Alert user
        end;

       If FileIsWritable(USF_WWW_HTACCESS) Then
         begin
          sList.SaveToFile(USF_WWW_HTACCESS); // Save new values to file
         end;

      sleep(100);
      sList.Free;                         // Remove from memory
     end;
    end;
{---------------------------------------------------------------------}

{====================================================================
This procedure sets who can gain access to the servers using
configuration file www\.htaccess

A radio button defines which section is executed.
  Local
  Local and Intranet Access
  Local, Intranet and Internet Access
=====================================================================}
procedure www_set_access_section;
var
    sList: TStringList;   // String list
    i:integer;            // Loop counter
begin
   // Load configuration file .htaccess
   if FileExists(USF_WWW_HTACCESS) Then
    begin
       sList := TStringList.Create;             // Create object
       sList.LoadFromFile(USF_WWW_HTACCESS);    // Load file

       // Enable Local Access
       If root_www_pass_access.RBtn_local.Checked Then
        begin
           //Scan list
           for i:=0 to sList.Count-1 do
            begin
               If Pos('Order Deny,Allow',sList[i]) <> 0 then
                 sList[i] := 'Order Deny,Allow';

               If Pos('Deny from all',sList[i]) <> 0 then
                 sList[i] := 'Deny from all';

               If Pos('Allow from 127.0.0.1',sList[i]) <> 0 then
                 sList[i] := 'Allow from 127.0.0.1';

               If Pos('Allow from ::1',sList[i]) <> 0 then
                 sList[i] := 'Allow from ::1';
            end;
        end;

       // Local and Intranet Access
       If root_www_pass_access.RBtn_local_intranet.Checked Then
        begin
           //Scan list
           for i:=0 to sList.Count-1 do
            begin
               If Pos('Order Deny,Allow',sList[i]) <> 0 then
                 sList[i] := 'Order Deny,Allow';

               If Pos('Deny from all',sList[i]) <> 0 then
                 sList[i] := 'Deny from all';

               If Pos('Allow from 127.0.0.1',sList[i]) <> 0 then
                 sList[i] := 'Allow from 127.0.0.1 192.168.0.0/16 172.16.0.0/12 10.0.0.0/8';

               If Pos('Allow from ::1',sList[i]) <> 0 then
                 sList[i] := 'Allow from ::1';
            end;
        end;

       // Local, Intranet and Internet Access
       If root_www_pass_access.RBtn_local_intranet_internet.Checked Then
        begin
           //Scan list
           for i:=0 to sList.Count-1 do
            begin
               If Pos('Order Deny,Allow',sList[i]) <> 0 then
                 sList[i] := '#Order Deny,Allow';

               If Pos('Deny from all',sList[i]) <> 0 then
                 sList[i] := '#Deny from all';

               If Pos('Allow from 127.0.0.1',sList[i]) <> 0  then
                 sList[i] := '#Allow from 127.0.0.1';

               If Pos('Allow from ::1',sList[i]) <> 0 then
                 sList[i] := '#Allow from ::1';
            end;
        end;

       If FileIsWritable(USF_WWW_HTACCESS) Then
          sList.SaveToFile(USF_WWW_HTACCESS); // Save new values to file

      sleep(100);
      sList.Free;                         // Remove from memory
     end;
    end;
{---------------------------------------------------------------------}

{ Troot_www_pass_access }

procedure Troot_www_pass_access.FormShow(Sender: TObject);
begin
   // 汉化界面
   Caption := 'www 根目录 - 更改密码和访问权限';
   GBox_enter_name_password.Caption := '输入名称和密码';
   Label_name.Caption := '名称';
   Label_password.Caption := '密码';
   Btn_add_to_list.Caption := '添加到列表 >>';
   Btn_delete_selected.Caption := '删除选中项';
   GBox_enable_disable_password.Caption := '启用或禁用密码';
   RBtn_password_disabled.Caption := '密码已禁用';
   RBtn_password_enabled.Caption := '密码已启用';
   GBox_access.Caption := 'Apache 服务器访问';
   RBtn_local.Caption := '本地访问';
   RBtn_local_intranet.Caption := '本地和局域网访问';
   RBtn_local_intranet_internet.Caption := '本地、局域网和互联网访问';
   
   //===Ensure htaccess file exists. Option for user to crate.
  if us_www_htaccess_check_create Then   // Check for htaccess file. Allow user to crate.
   begin
     //Create back up
     If Not FileExists(USF_WWW_HTACCESS_BACK) Then
       CopyFile(USF_WWW_HTACCESS,USF_WWW_HTACCESS_BACK);  // Create backup file

     www_access_init; // Set initial or reload and displlay current status

   end
  Else    //File does not exits. User skipped option to create
   begin
     root_www_pass_access.Close;        // Nothing else to do
   end;
end;

procedure Troot_www_pass_access.RBtn_localClick(Sender: TObject);
begin
     www_set_access_section; // Set server access block
end;

procedure Troot_www_pass_access.RBtn_local_intranetClick(Sender: TObject);
begin
     www_set_access_section; // Set server access block
end;

procedure Troot_www_pass_access.RBtn_local_intranet_internetClick(
  Sender: TObject);
begin
     www_set_access_section; // Set server access block
end;

procedure Troot_www_pass_access.RBtn_password_disabledClick(Sender: TObject);
begin
    www_enable_disable_password_section; // Disable password block
end;

procedure Troot_www_pass_access.RBtn_password_enabledClick(Sender: TObject);
begin
    If Not password_file_contains_defaults(USF_WWW_PASSWORD) Then
     begin
        If Not password_file_empty(USF_WWW_PASSWORD) Then
          www_enable_disable_password_section; // Enable password block
     end;
    www_access_init; // Set initial or reload and displlay current status
 end;

procedure Troot_www_pass_access.Btn_add_to_listClick(Sender: TObject);
{Add entered name and password to list and save list to file}
 Var
   new_name_password:string;
   sList: TStringList;       // String list
begin
   If (Edit_name.Text = '') or  (Edit_password.Text ='') Then
    begin
     // Display warning mesage Both Name and Password required
      us_MessageDlg('错误','名称和密码都是必需的', mtWarning,[mbOk],0) ;
    end
   else
   begin
   new_name_password := Edit_name.Text + ':' + Edit_password.Text; // Get new name-password pair
   ListBox1.Items.Add(new_name_password);                          // Add pair to listbox

   sList := TStringList.Create;            // Create object
   sList.Assign(ListBox1.Items) ;          // Add items to string list


  If FileIsWritable(USF_WWW_PASSWORD) Then
     sList.SaveToFile(USF_WWW_PASSWORD);     // Save new values to file

  sleep(100);
  sList.Free;                             // Remove from memory

  Edit_name.Text      :=''; // Clear entery
  Edit_password.Text  :=''; // Clear entery
 end;
end;

procedure Troot_www_pass_access.Btn_delete_selectedClick(Sender: TObject);
{Delete selected Name Password entry update list and file}
var
   i:integer;
   sList: TStringList;       // String list
begin
   // Delete selected entry
   For i := ListBox1.Items.Count - 1 downto 0 do
    if ListBox1.Selected [i] then
     ListBox1.Items.Delete (i);

   // Update file
   sList := TStringList.Create;            // Create object
   sList.Assign(ListBox1.Items) ;          // Add items to string list

  If FileIsWritable(USF_WWW_PASSWORD) Then
     sList.SaveToFile(USF_WWW_PASSWORD);     // Save new values to file

  sleep(100);
  sList.Free;                             // Remove from memory

end;


end.

