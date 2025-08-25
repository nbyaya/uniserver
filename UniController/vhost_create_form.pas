unit vhost_create_form;

{#############################################################################
'# Name: vhost_create_form.pas
'# Developed By: The Uniform Server Development Team
'# Web: https://www.uniformserver.com
'#############################################################################}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  default_config_vars,
  us_common_procedures,
  us_common_functions,
  RegExpr;

type

  { Tvhost_create }

  Tvhost_create = class(TForm)
    Btn_create_vhost: TButton;
    Btn_help_server_name: TButton;
    Btn_help_root_folder: TButton;
    Edit_root_folder: TEdit;
    Edit_server_name: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure Btn_create_vhostClick(Sender: TObject);
    procedure Btn_help_root_folderClick(Sender: TObject);
    procedure Btn_help_server_nameClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  vhost_create: Tvhost_create;

implementation

{$R *.lfm}

{===============================================================
Add Vhost block to Vhost configuration file:
Input In_RootFolder
Input In_ServerName

==File httpd-vhosts.conf:
 1) Check file exists
 2) Check for existing Vhost section
 3) Add new Vhost section
 4) Remove duplicated blank lines
 5) Save file

----------------------------------------------------------------}
procedure add_vhost_block(In_RootFolder:String;In_ServerName:String);
var
 vhost_found :boolean;     // Vhost section found
 sList       :TStringList; // String list
 i           :integer;     // Loop counteri
begin
  vhost_found     := False;                 // Assume no vhost section

  If FileExists(USF_APACHE_VHOST_CNF) Then   // Check file exists
  begin
   sList  := TStringList.Create;             // Create object
   sList.LoadFromFile(USF_APACHE_VHOST_CNF); // Load Apache Vhost config file

   for i:=0 to sList.Count-1 do
     begin
        //-- Check section not present
        If (sList[i]<>'') and ExecRegExpr('^\s*ServerName\s*'+In_ServerName, sList[i]) Then
         begin
           vhost_found := True; //Already contains the new Vhost section
           break;               //Nothing else to do
          end;
      end;//End scan new list

   //--Add new section
   If Not vhost_found Then
     begin
       sList.Add('');
       sList.Add('<VirtualHost *:${AP_PORT}>');
       sList.Add(' ServerAdmin webmaster@'+In_ServerName);
       sList.Add(' DocumentRoot ${US_ROOTF}/vhosts/'+In_RootFolder);
       sList.Add(' ServerName '+ In_ServerName);
       sList.Add(' ServerAlias www.'+IN_ServerName+ ' *.'+In_ServerName);
       sList.Add(' ErrorLog logs/'+In_ServerName+'-error.log');
       sList.Add(' CustomLog logs/'+In_ServerName+'-access.log common');
       sList.Add(' <Directory "${HOME}\vhosts\'+In_RootFolder+'">');
       sList.Add('   Options Indexes Includes ');
       sList.Add('   AllowOverride All   ');
       sList.Add('   Require all granted ');
       sList.Add(' </Directory> ');
       sList.Add('</VirtualHost> ');
       sList.Add('');

       //==Clean file list

       //Remove all blank lines
       for i:=sList.Count-1 downto 0 do
         If sList[i]='' Then
           sList.Delete(i); //Delete entery

       //Insert blank line above start of each Vhost block
       for i:=sList.Count-1 downto 0 do
         begin
           //-- Get Start of Vhost
           If (sList[i]<>'') and ExecRegExpr('^\s*<VirtualHost\s', sList[i]) Then
              sList.Insert(i,''); // Insert blank line
         end;

       //Save new Vhost file
       If FileIsWritable(USF_APACHE_VHOST_CNF) Then
          sList.SaveToFile(USF_APACHE_VHOST_CNF); // Save Vhost file

     end;

   //Clean up
   sList.Free;      // Remove from memory
  end;//End Enable Vhost
end;
{--------------------------------------------------------------}

{ Tvhost_create }

procedure Tvhost_create.Btn_create_vhostClick(Sender: TObject);
{
Create new Vhost:
  Environment variable HOME=C:\UniserverZ
  Environment variable US_ROOTF=C:/UniserverZ
}
var
   new_ServerName  :string;      // New server name - Domain name
   new_root_folder :string;      // Server root folder - Folder name only
   new_root_path   :string;      // Full root path inclding new_root_folder
   sList           :TStringList; // String list
   valid_input     :boolean;     // Valid data from user
   i               :integer;     // Loop counteri
begin
   new_root_path   :='';
   new_root_folder := Edit_root_folder.Text; // Get root-folder entered
   new_ServerName  := Edit_server_name.Text; // Get server name entered

   valid_input := True;    // Assume input is invalid

   //==Check data entered by user - Validate user input===

   //--Check Root folder
   If Not valid_root_folder_name(new_root_folder,'Root folder') Then valid_input := False;

   //--Check domain name looks resonable e.g fred.com
   If Not valid_server_name(new_ServerName,'Server Name')       Then valid_input := False;

  //===Create full root path
  If valid_input Then
    new_root_path := UniConPath+'\vhosts\'+new_root_folder;

  //###== Create new Vhost ==###
  If valid_input Then
   begin

    //==File httpd.conf enable Vhost include section:
    If FileExists(USF_APACHE_CNF) Then       // Check file exists
    begin
     sList  := TStringList.Create;           // Create object
     sList.LoadFromFile(USF_APACHE_CNF);     // Load Apache main config file

     for i:=0 to sList.Count-1 do
       begin
          //-- Check section not enabled
          If (sList[i]<>'') and ExecRegExpr('^\s*#\s*Include\s*conf/extra/httpd-vhosts.conf', sList[i]) Then
           begin
             sList[i] := 'Include conf/extra/httpd-vhosts.conf'; // Enabled line (section)
             //Save updated file
             If FileIsWritable(USF_APACHE_CNF) Then
                sList.SaveToFile(USF_APACHE_CNF);     // Save new values to file

              sleep(100);
              break; //Nothing else to do
            end;
        end;//End scan new list

     //Clean up
     sList.Free;      // Remove from memory
    end;//End Enable Vhost

    //== Create new document root folder
    If Not DirectoryExists(new_root_path) Then
       ForceDirectories(new_root_path);   // If directory does not exist create it

    //== Copy .htaccess and favicon.ico files to new root folder
    If not FileExists(new_root_path+'\.htaccess') then
       CopyFile(USF_VHOST_HTACCESS, new_root_path+'\.htaccess'); // .htaccess file for new Vhost


    If not FileExists(new_root_path+'\favicon.ico') then
       CopyFile(USF_VHOST_ICO,new_root_path+'\favicon.ico'); // favicon image for new Vhost

     //===Add new block to Vhost configuration file
     add_vhost_block(new_root_folder,new_ServerName);        // Add new Vhost to configuration file

     //===Add to Uniform Server PAC file
     us_add_to_pac_file(new_ServerName);                     // Add new host to PAC file

     //===Add to Windows hosts  file
     us_add_to_hosts_file(new_ServerName);                   // Add new host to hosts file

     //Inform user
     us_MessageDlg('Apache 信息','新虚拟主机已创建', mtcustom,[mbOk],0) ; //Display information message
  end;//End valid_input

  If valid_input Then vhost_create.Close;

end;

procedure Tvhost_create.Btn_help_root_folderClick(Sender: TObject);
var
  str:string;
begin
    str :='';
    str := str + '文档根目录是文件夹的完整路径。'         + sLineBreak + sLineBreak;

    str := str + '您只需要指定文件夹名称。'           + sLineBreak+ sLineBreak;

    str := str + '此文件夹名称将添加到以下文件夹路径：'         + sLineBreak;
    str := str + ' '+ US_VHOSTS+'\' + sLineBreak;
    str := str + 'Apache 将从此文件夹提供主机网站服务。'  + sLineBreak + sLineBreak;

    str := str + '注意 1：'  + sLineBreak;
    str := str + '为了便携性，所有虚拟主机根文件夹都创建在文件夹中：'  + sLineBreak;
    str := str + ' '+ US_VHOSTS+'\' + sLineBreak + sLineBreak;

    str := str + '注意 2：'  + sLineBreak;
    str := str + '或者，对于固定安装，您可以编辑文件：'  + sLineBreak;
    str := str +  USF_APACHE_VHOST_CNF  + sLineBreak;
    str := str + '根据需要更改 DocumentRoot 和 Directory 路径。';


   us_MessageDlg('文档根目录', str, mtInformation,[mbOk],0) ; //Display message


end;

procedure Tvhost_create.Btn_help_server_nameClick(Sender: TObject);
var
   str:string;
begin
  str := '';
  str := str + '主机名是您在浏览器中输入的地址，' + sLineBreak;
  str := str + '不包括 http:// 部分。' + sLineBreak + sLineBreak;

  str := str + '示例 1：' + sLineBreak;
  str := str + '完整互联网地址：http://www.me.com' + sLineBreak;
  str := str + '主机名：www.me.com' + sLineBreak + sLineBreak;

  str := str + '示例 2：' + sLineBreak;
  str := str + '完整互联网地址：http://uniserver.com' + sLineBreak;
  str := str + '主机名：uniserver.com';

  us_MessageDlg('服务器名称 - 主机名', str, mtInformation,[mbOk],0); //Display message
end;

procedure Tvhost_create.FormShow(Sender: TObject);
begin
   // 汉化界面
   Caption := '创建 Apache 虚拟主机';
   Label1.Caption := '根文件夹名称：';
   Label2.Caption := '服务器名称：';
   Label3.Caption := '例如：fred123';
   Label4.Caption := '例如：fred.com';
   Btn_create_vhost.Caption := '创建虚拟主机';
   
   Edit_root_folder.Text :='';
   Edit_server_name.Text :='';
end;

end.

