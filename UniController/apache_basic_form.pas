unit apache_basic_form;

{#############################################################################
'# Name: default_config_vars.pas
'# Developed By: The Uniform Server Development Team
'# Web: https://www.uniformserver.com
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  windows,
  us_server_state,
  default_config_vars,
  us_common_functions,
  us_common_procedures,
  RegExpr;

type

  { Tapache_basic }

  Tapache_basic = class(TForm)
    Btn_update_config: TButton;
    Btn_help: TButton;
    Com_lua_module: TComboBox;
    Com_plua_module: TComboBox;
    Com_server_signature: TComboBox;
    Edit_server_name: TEdit;
    Edit_email: TEdit;
    Edit_index_files: TEdit;
    Edit_ssi: TEdit;
    Editl_listen_port: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Lbl_server_name: TLabel;
    Llb_email: TLabel;
    Lbl_index_files: TLabel;
    Lbl_ssi: TLabel;
    Lbl_server_signature: TLabel;
    Lbl_listen_port: TLabel;
    procedure Btn_helpClick(Sender: TObject);
    procedure Btn_update_configClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  apache_basic: Tapache_basic;

implementation

{$R *.lfm}

{ Tapache_basic }

procedure Tapache_basic.FormShow(Sender: TObject);
var
  sList: TStringList;   // String list
  RegexObj: TRegExpr;   // Object
  i:integer;            // loop counter
begin
  // 汉化界面
  Caption := 'Apache 基本配置';
  GroupBox1.Caption := '常用配置参数';
  Lbl_server_name.Caption := '服务器名称';
  Llb_email.Caption := '服务器管理员邮箱';
  Lbl_index_files.Caption := '目录索引文件';
  Lbl_ssi.Caption := '服务器端包含';
  Lbl_server_signature.Caption := '服务器签名';
  Lbl_listen_port.Caption := '监听端口';
  Label1.Caption := 'Lua 模块';
  Label2.Caption := 'pLua 模块';
  Btn_update_config.Caption := '更新配置';
  Btn_help.Caption := '帮助';

  //=== Get data from environment variables

    //-- Get Server name
    Edit_server_name.Text  := UENV_US_SERVERNAME; // Display server name

    //-- Get Apache listen port
    Editl_listen_port.Text := UENV_AP_PORT;       // Display port value


  //=== Get data from configuration file httpd.conf
  if FileExists(USF_APACHE_CNF) Then
   begin
      sList := TStringList.Create;           // Create object
      sList.LoadFromFile(USF_APACHE_CNF);    // Load file
      RegexObj := TRegExpr.Create;           // Create regex obj

      //Scan list
      for i:=0 to sList.Count-1 do
       begin

        //-- Get Server Admin Email
        RegexObj.Expression := '^\s*ServerAdmin\s*([\w@.\$\{\}]+)'; // Set search pattern
         if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                            // Match found
           begin
            //Expand email may contain ${US_SERVERNAME}
            Edit_email.Text := StringReplace(RegexObj.Match[1], '${US_SERVERNAME}',UENV_US_SERVERNAME,[rfReplaceAll]);
            //Edit_email.Text := RegexObj.Match[1];                   // Display Admin email
           end;

         //-- Get Directory Index Files
         RegexObj.Expression := '^\s*DirectoryIndex\s*([\w.\s]+[^\s])\s*$'; // Set search pattern
          if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                                   // Match found
            begin
             Edit_index_files.Text := RegexObj.Match[1];         // Display index file extensions             end;
            end;

          //-- Get Lua status
           RegexObj.Expression := '^#LoadModule lua_module modules/mod_lua.so'; //Set search pattern
             if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                                    // Match found
                Com_lua_module.Text  := 'Disabled';      //Display Lua status

           RegexObj.Expression := '^LoadModule lua_module modules/mod_lua.so';  //Set search pattern
             if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                                    // Match found
               Com_lua_module.Text   := 'Enabled';       //Display Lua status

          //-- Get PLua status
             RegexObj.Expression := '^#LoadModule plua_module modules/mod_plua.so'; //Set search pattern
               if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                                     // Match found
                  Com_plua_module.Text  := 'Disabled';      //Display Lua status

             RegexObj.Expression := '^LoadModule plua_module modules/mod_plua.so';  //Set search pattern
               if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                                     // Match found
                 Com_plua_module.Text   := 'Enabled';       //Display Lua status


       end;
      sList.Free;                         // Remove from memory
      RegexObj.Free;                      // release object
   end;

  //=== Get data from configuration file httpd-default.conf
  if FileExists(USF_APACHE_DEFAULT_CNF) Then
   begin
      sList := TStringList.Create;                 // Create object
      sList.LoadFromFile(USF_APACHE_DEFAULT_CNF);  // Load file
      RegexObj := TRegExpr.Create;                 // Create regex obj

      //Scan list
      for i:=0 to sList.Count-1 do
       begin

        //-- Get server-parsed ssi file extensions
        RegexObj.Expression := '^\s*AddHandler\s*server-parsed\s*([\w.\s]+[^\s])\s*$'; //Set search pattern
          if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                                               // Match found
            begin
             Edit_ssi.Text := RegexObj.Match[1];  //Display list of file extensions
            end;

         //-- Get Server signature ststus
          RegexObj.Expression := '^\s*ServerSignature\s*(\w+)'; //Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                     // Match found
              begin
               Com_server_signature.Text := RegexObj.Match[1]; //Display server signature ststus
              end;
       end;
       sList.Free;     // Remove from memory
      RegexObj.Free;   // release object
   end;

   //-- Check PLua module
   if FileExists(US_APACHE_MODULES + '\mod_pLua.so') Then
     Com_plua_module.Enabled := True
   else
     begin
       Com_plua_module.Text    := 'Module mod_pLua.so not found';
       Com_plua_module.Enabled := False;
     end;
end;

procedure Tapache_basic.Btn_helpClick(Sender: TObject);
var
 str:string;
begin
  //str_ab_help_title = Basic Apache Configuration
  str :='';
  str := str + '此表单允许您更改常用的' + sLineBreak;
  str := str + 'Apache 配置选项。' + sLineBreak + sLineBreak;
  str := str + '要编辑其他 Apache 选项，请从控制器菜单中' + sLineBreak;
  str := str + '选择编辑 httpd.conf。' + sLineBreak;
  us_MessageDlg('Apache 基本配置', str, mtInformation,[mbOk],0) ; //Display message
end;

procedure Tapache_basic.Btn_update_configClick(Sender: TObject);
var
 sList            : TStringList; // String list
 RegexObj         : TRegExpr;    // Object
 i                : integer;     // loop counter
 new_admin_email  : String;
 valid_input      : boolean;

begin
  valid_input := True;    // Assume input is invalid

 //==Check data entered by user - Validate user input===

   //--Check domain name (server name) looks resonable e.g fred.com
   If Not valid_server_name(Edit_server_name.Text,'服务器名称')     Then valid_input := False;

   //--Check Server port - standard port 80
   If Not valid_server_port(Editl_listen_port.Text,'服务器端口')    Then valid_input := False;

   //--Check Admin E-mail for correct format
   If Not valid_admin_email(Edit_email.Text,'管理员邮箱')          Then valid_input := False;

   //--Check Directory index files for correct format
   If Not valid_directory_index_files(Edit_index_files.Text,'目录索引文件列表') Then valid_input := False;

   //--Check Server parsed ssi file extensions for correct format
   If Not valid_ssi_file_extensions(Edit_ssi.Text,'SSI 文件扩展名') Then valid_input := False;

   //--Check Server Signature On/Off
   If Not valid_server_signature(Com_server_signature.Text,'服务器签名') Then valid_input := False;

  //==Valid input update configuration===
  If valid_input Then
   begin

    //-- Set Server name
    UENV_US_SERVERNAME := Edit_server_name.Text;                               // Set var
    us_ini_set(USF_US_USER_INI,'USER','US_SERVERNAME',UENV_US_SERVERNAME);     // save to config file
    windows.SetEnvironmentVariable('US_SERVERNAME',PCHAR(UENV_US_SERVERNAME)); // Set environoment variable
    us_add_to_pac_file(UENV_US_SERVERNAME);                                    // Add new host to PAC file set ip 127.0.0.1
    us_add_to_hosts_file(UENV_US_SERVERNAME);                                  // Add new host to hosts file set ip 127.0.0.1
    us_update_server_state;                                                    // Update menus

    //-- Set Apache listen port
    UENV_AP_PORT := Editl_listen_port.Text;                        // Set var
    us_ini_set(USF_US_USER_INI,'USER','AP_PORT',UENV_AP_PORT);     // save to config file
    windows.SetEnvironmentVariable('AP_PORT',PCHAR(UENV_AP_PORT)); // Set environoment variable
    us_update_server_state;                                        // Update menus

    //=== Save new settings to configuration file httpd.conf
    if FileExists(USF_APACHE_CNF) Then
     begin
        sList := TStringList.Create;        // Create object
        sList.LoadFromFile(USF_APACHE_CNF); // Load file
        RegexObj := TRegExpr.Create;        // Create regex obj

        //Scan list
        for i:=0 to sList.Count-1 do
         begin

           //-- Set Server Admin Email
           if (sList[i]<>'') and ExecRegExpr('^\s*ServerAdmin\s*', sList[i]) then  // Match found
             begin
               new_admin_email := Edit_email.Text;    //Get user input
               new_admin_email := StringReplace(new_admin_email, UENV_US_SERVERNAME,'${US_SERVERNAME}',[rfReplaceAll]);
               sList[i] :=  'ServerAdmin '+new_admin_email;
             end;

           //-- Set Directory Index Files
           if (sList[i]<>'') and ExecRegExpr('^\s*DirectoryIndex\s*', sList[i]) then  // Match found
             begin
                sList[i] := '    DirectoryIndex '+Edit_index_files.Text;
             end;

           //-- Set Lua status
            RegexObj.Expression := '^?LoadModule lua_module modules/mod_lua.so'; //Set search pattern
              if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                                    // Match found
               begin
                 If Com_lua_module.Text = 'Enabled' Then
                   sList[i] := 'LoadModule lua_module modules/mod_lua.so'
                 Else
                   sList[i] := '#LoadModule lua_module modules/mod_lua.so'
               end;

           //-- Set PLua status
            RegexObj.Expression := '^?LoadModule plua_module modules/mod_plua.so'; //Set search pattern
              if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                                      // Match found
               begin
                 If Com_plua_module.Text = 'Enabled' Then
                   sList[i] := 'LoadModule plua_module modules/mod_plua.so'
                 Else
                   sList[i] := '#LoadModule plua_module modules/mod_plua.so'
               end;




         end;//Ends scan list

        //Save config file
        If FileIsWritable(USF_APACHE_CNF) Then
           sList.SaveToFile(USF_APACHE_CNF);  // Save new values to file

        //Cleanup
        sList.Free;                        // Remove from memory
        RegexObj.Free;                     // release object
     end;//End FileExists(USF_APACHE_CNF)


   //=== Save new settings to configuration httpd-default.conf
  if FileExists(USF_APACHE_DEFAULT_CNF) Then
   begin
      sList := TStringList.Create;        // Create object
      sList.LoadFromFile(USF_APACHE_DEFAULT_CNF); // Load file
      RegexObj := TRegExpr.Create;        // Create regex obj

      //Scan list
      for i:=0 to sList.Count-1 do
       begin

        //-- Set server-parsed ssi file extensions
        if (sList[i]<>'') and ExecRegExpr('^\s*AddHandler\s*server-parsed\s*', sList[i]) then  // Match found
         begin
          sList[i] := 'AddHandler server-parsed '+Edit_ssi.Text;
         end;

        //-- Set Server signature ststus
        if (sList[i]<>'') and ExecRegExpr('^\s*ServerSignature\s*', sList[i]) then  // Match found
          begin
            sList[i] := 'ServerSignature '+Com_server_signature.Text;
          end;

       end;//Ends scan list

      //Save config file
      If FileIsWritable(USF_APACHE_DEFAULT_CNF) Then
        sList.SaveToFile(USF_APACHE_DEFAULT_CNF);  // Save new values to file

      //Cleanup
      sList.Free;                        // Remove from memory
      RegexObj.Free;                     // release object
   end;//End FileExists(USF_APACHE_DEFAULT_CNF)

   //Inform user
   us_MessageDlg('Apache 配置', 'Apache 配置文件已更新', mtInformation,[mbOk],0) ; //Display message

   //Close form
   If valid_input Then apache_basic.Close;

   end;//End valid_input
   //=== Set environment variables

end;

end.

