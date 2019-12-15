program ConexusTS;

{$mode objfpc}{$H+}
{$MACRO ON}

{____________________________________________________________
|  _______________________________________________________  |
| |                                                       | |
| |           Remote for TechniSat based devices          | |
| | (c) 2019 Alexander Feuster (alexander.feuster@web.de) | |
| |             http://www.github.com/feuster             | |
| |_______________________________________________________| |
|___________________________________________________________}

//define program basics
{$DEFINE PROGVERSION:='1.1'}
//{$DEFINE PROG_DEBUG}
{___________________________________________________________}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, StrUtils, Keyboard, TechniSatAPI
  { you can add units after this };

type

  { TApp }

  TApp = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure HelpHint; virtual;
    procedure WaitPrint; virtual;
    procedure WaitClear; virtual;
    procedure ButtonRequest(URL: String; PIN: String; Button: Byte; INT_Timeout: Integer); virtual;
  end;

const
  //program title
  STR_Title:    String = ' __________________________________________________ '+#13#10+
                         '|  ______________________________________________  |'+#13#10+
                         '| |                                              | |'+#13#10+
                         '| |**********************************************| |'+#13#10+
                         '| |      Remote for TechniSat based devices      | |'+#13#10+
                         '| |          (c) 2019 Alexander Feuster          | |'+#13#10+
                         '| |        http://www.github.com/feuster         | |'+#13#10+
                         '| |______________________________________________| |'+#13#10+
                         '|__________________________________________________|'+#13#10;

  //program version
  STR_Version:    String = PROGVERSION;

  //CPU architecture
  STR_CPU:      String = {$I %FPCTARGETCPU%};

  //Build info
  STR_Build:    String = {$I %FPCTARGETOS%}+' '+{$I %FPCTARGETCPU%}+' '+{$I %DATE%}+' '+{$I %TIME%};
  {$WARNINGS OFF}
  STR_User:     String = {$I %USER%};
  {$WARNINGS ON}
  STR_Date:     String = {$I %DATE%};

  //Message strings
  STR_Info:         String = 'Info:    ';
  STR_Error:        String = 'Error:   ';
  STR_Warning:      String = 'Warning: ';
  STR_WaitingMsg:   String = 'Please wait...';
  {$IFDEF PROG_DEBUG}
  STR_Debug:        String = 'Debug:   ';
  {$ENDIF}

  //Timeout
  INT_Timeout:      Integer = 100;
  INT_Timeout_Max:  Integer = 1000;

var
  STR_Title_Banner: String;

{ ConexusTS }

procedure TApp.DoRun;
var
  ErrorMsg:   String;
  URL:        String;
  PIN:        String;
  Command:    String;
  Command2:   String;
  ButtonState:String;
  Buffer:     String;
  Buffer2:    Byte;
  Buffer3:    TStringList;
  Buffer4:    Integer;
  Buffer5:    Integer;
  Buffer6:    Integer;
  Buffer7:    TKeyEvent;

begin
  //init variables
  ErrorMsg:='';
  URL:='';
  PIN:='';
  Command:='';
  Command2:='';
  ButtonState:='';
  Buffer:='';
  Buffer2:=0;
  Buffer4:=0;
  Buffer5:=0;
  Buffer6:=0;

  //add CPU architecture info to title
  if STR_CPU='x86_64' then
    {$IFDEF PROG_DEBUG}
    STR_Title_Banner:=StringReplace(STR_Title,'**********************************************','         ConexusTS V'+STR_Version+' Debug (64Bit)         ',[])
    {$ELSE}
    STR_Title_Banner:=StringReplace(STR_Title,'**********************************************','            ConexusTS V'+STR_Version+' (64Bit)            ',[])
    {$ENDIF}
  else if STR_CPU='i386' then
    {$IFDEF PROG_DEBUG}
    STR_Title_Banner:=StringReplace(STR_Title,'**********************************************','         ConexusTS V'+STR_Version+' Debug (32Bit)         ',[])
    {$ELSE}
    STR_Title_Banner:=StringReplace(STR_Title,'**********************************************','            ConexusTS V'+STR_Version+' (32Bit)            ',[])
    {$ENDIF}
  else
    {$IFDEF PROG_DEBUG}
    STR_Title_Banner:=StringReplace(STR_Title,'**********************************************','            ConexusTS V'+STR_Version+' Debug              ',[]);
    {$ELSE}
    STR_Title_Banner:=StringReplace(STR_Title,'**********************************************','                ConexusTS V'+STR_Version+'                ',[]);
    {$ENDIF}

  // quick check parameters
  ErrorMsg:=CheckOptions('hbalnidxu:p:c:s:t:r:oy', 'help build api license nobanner showbanner devicelist rcucommands url: pin: command: sendbutton: buttonstate: buttonrepeat: loop loopcommands');
  if (ErrorMsg<>'') or (ParamCount=0) then
    begin
      //write title banner
      WriteLn(STR_Title_Banner);
      if ParamCount>0 then
        WriteLn(STR_Error+ErrorMsg)
      else
        WriteLn(STR_Error+'No arguments!');
      HelpHint;
      Terminate;
      Exit;
    end;

  // parse parameters

  //show banner if not surpressed
  if (HasOption('n', 'nobanner')=false) or (HasOption('i', 'showbanner')=true) then
    WriteLn(STR_Title_Banner);

  // parse parameters
  if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

  //show build info
  if HasOption('b', 'build') then
    begin
      if STR_User<>'' then
        {$IFDEF PROG_DEBUG}
        WriteLn(STR_Info,'Build "V'+STR_Version+' '+STR_Build+'" (DEBUG) compiled by "'+STR_User+'"')
        {$ELSE}
        WriteLn(STR_Info,'Build "V'+STR_Version+' '+STR_Build+'" compiled by "'+STR_User+'"')
        {$ENDIF}
      else
        {$IFDEF PROG_DEBUG}
        WriteLn(STR_Info,'Build "V'+STR_Version+' (DEBUG) '+STR_Build+'"');
        {$ELSE}
        WriteLn(STR_Info,'Build "V'+STR_Version+' '+STR_Build+'"');
        {$ENDIF}
      Terminate;
      Exit;
    end;

  //show API info
  if HasOption('a', 'api') then
    begin
      if TSAPI_DEBUG then
        WriteLn(STR_Info,'TechniSat API V'+API_Version+' (Debug)')
      else
        WriteLn(STR_Info,'TechniSat API V'+API_Version);
      Terminate;
      Exit;
    end;

  //show license info
  if HasOption('l', 'license') then
    begin
      //show Conexus license
      WriteLn('ConexusTS V'+STR_Version+' (c) '+STR_Date[1..4]+' Alexander Feuster (alexander.feuster@web.de)'+#13#10+
              'http://www.github.com/feuster'+#13#10+
              'This program is provided "as-is" without any warranties for any data loss,'+#13#10+
              'device defects etc. Use at own risk!'+#13#10+
              'Free for personal use. Commercial use is prohibited without permission.'+#13#10);
      //show API license
      Write(API_License);
      Terminate;
      Exit;
    end;

  //list in network available TechniSat devices
  if HasOption('d', 'devicelist') then
    begin
      WriteLn(STR_Info,'Network broadcast scan for available TechniSat devices');
      WaitPrint;
      Buffer3:=TStringList.Create;
      Buffer3:=tsapi_Info_DeviceList(INT_Timeout_Max);
      WaitClear;
      if Buffer3.Count=0 then
        begin
          WriteLn(STR_Info,'First try did not found any available devices. Starting second try.');
          Buffer3:=tsapi_Info_DeviceList(INT_Timeout_Max);
          WaitClear;
          if Buffer3.Count=0 then
            begin
              WriteLn(STR_Info,'No devices available!')
            end
          else
            begin
              if Buffer3.Count>0 then
                begin
                  WriteLn('');
                  WriteLn('       IP      |                Name                |  Type  |     Serial     | Version  ');
                  WriteLn('---------------|------------------------------------|--------|----------------|----------');
                  for Buffer2:=0 to Buffer3.Count-1 do
                    begin
                      WriteLn(Format('%0:-15s',[Buffer3.Strings[Buffer2].Split('|')[0]]):15,'|',Format('%0:-36s',[Buffer3.Strings[Buffer2].Split('|')[1]]):36,'|',Format('%0:-8s',[Buffer3.Strings[Buffer2].Split('|')[2]]):8,'|',Format('%0:-16s',[Buffer3.Strings[Buffer2].Split('|')[3]]):16,'|',Format('%0:-10s',[Buffer3.Strings[Buffer2].Split('|')[4]]):10);
                    end;
                end;
            end;
        end
      else
        begin
          if Buffer3.Count>0 then
            begin
              WriteLn('');
              WriteLn('       IP      |                Name                |  Type  |     Serial     | Version  ');
              WriteLn('---------------|------------------------------------|--------|----------------|----------');
              for Buffer2:=0 to Buffer3.Count-1 do
                begin
                  WriteLn(Format('%0:-15s',[Buffer3.Strings[Buffer2].Split('|')[0]]):15,'|',Format('%0:-36s',[Buffer3.Strings[Buffer2].Split('|')[1]]):36,'|',Format('%0:-8s',[Buffer3.Strings[Buffer2].Split('|')[2]]):8,'|',Format('%0:-16s',[Buffer3.Strings[Buffer2].Split('|')[3]]):16,'|',Format('%0:-10s',[Buffer3.Strings[Buffer2].Split('|')[4]]):10);
                end;
            end;
        end;
      {$IFDEF PROG_DEBUG}
      WriteLn('');
      writeln(STR_Debug,'DEVICELIST -> RESPONSE BEGIN');
      writeln('--------------------------------------------------------------------------------');
      Writeln(Buffer3.Text);
      writeln('--------------------------------------------------------------------------------');
      writeln(STR_Debug,'DEVICELIST -> RESPONSE END'+#13#10+#13#10);
      {$ENDIF}
      Terminate;
      Exit;
    end;

  //show RCU command list
  if HasOption('x', 'rcucommands') then
    begin
      WriteLn('');
      WriteLn(' Code |        Name        |          Description         ');
      WriteLn('------|--------------------|------------------------------');
      for Buffer2:=0 to Length(tsapi_Buttons)-1 do
        begin
          writeln(Format('%0:-6s',[IntToStr(tsapi_Buttons[Buffer2].Code)]):6,'|',Format('%0:-20s',[tsapi_Buttons[Buffer2].Name]):20,'|',Format('%0:-30s',[tsapi_Buttons[Buffer2].Description]):30);
        end;
      Terminate;
      Exit;
    end;

  //show loop command list
  if HasOption('y', 'loopcommands') then
    begin
      WriteLn('');
      WriteLn(' Key Name | Function ');
      WriteLn('----------|----------');
      WriteLn('0         |Button 0');
      WriteLn('1         |Button 1');
      WriteLn('2         |Button 2');
      WriteLn('3         |Button 3');
      WriteLn('4         |Button 4');
      WriteLn('5         |Button 5');
      WriteLn('6         |Button 6');
      WriteLn('7         |Button 7');
      WriteLn('8         |Button 8');
      WriteLn('9         |Button 9');
      WriteLn('UP        |Up');
      WriteLn('DOWN      |Down');
      WriteLn('LEFT      |Left');
      WriteLn('RIGHT     |Right');
      WriteLn('ENTER     |OK');
      WriteLn('PAGE UP   |Page up');
      WriteLn('PAGE DOWN |Page down');
      WriteLn('INSERT    |Program up');
      WriteLn('DELETE    |Program down');
      WriteLn('A         |Audio');
      WriteLn('B         |Back/Exit');
      WriteLn('E         |EPG/SFI');
      WriteLn('H         |HDMI');
      WriteLn('I         |Info');
      WriteLn('L         |TV/Radio list');
      WriteLn('M         |Menu');
      WriteLn('N         |Navigation');
      WriteLn('O         |Option');
      WriteLn('P         |PiP/PaP');
      WriteLn('R         |Record');
      WriteLn('S         |Standby');
      WriteLn('T         |Teletext');
      WriteLn('V         |Volume mute');
      WriteLn('X         |Timer');
      WriteLn('Y         |Still picture');
      WriteLn('Z         |Zoom');
      WriteLn('F1        |Red');
      WriteLn('F2        |Green');
      WriteLn('F3        |Yellow');
      WriteLn('F4        |Blue');
      WriteLn('F5        |Rewind');
      WriteLn('F6        |Stop');
      WriteLn('F7        |Play/Pause');
      WriteLn('F8        |Forward wind');
      WriteLn(STR_Info+'All key mappings depend on a QWERTZ keyboard layout.');
      Terminate;
      Exit;
    end;

  //check for existing command
  if HasOption('c', 'command') then
    begin
      if HasOption('c', 'command') then
        begin
          Command2:=GetOptionValue('c', 'command');
          Command:=UpperCase(Command2);
        end;
    end
  else
    begin
      if not ((HasOption('s', 'sendbutton')) or (HasOption('o', 'loop'))) then
        begin
          WriteLn(STR_Error+'No command specified');
          HelpHint;
          Terminate;
          Exit;
        end;
    end;

  //check URL
  if HasOption('u', 'url') then
    begin
      URL:=(GetOptionValue('u', 'url'));
      if AnsiPos('http://',LowerCase(URL))>0 then
        URL:=StringReplace(URL,'http://','',[rfReplaceAll, rfIgnoreCase]);
      if AnsiPos('https://',LowerCase(URL))>0 then
        URL:=StringReplace(URL,'https://','',[rfReplaceAll, rfIgnoreCase]);
      if AnsiLeftStr(URL,1)='@' then
        begin
          URL:=AnsiRightStr(URL,Length(URL)-1);
          WriteLn(STR_Info+'Device name or serial instead of URL specified. Searching for IP.');
          URL:=tsapi_Info_GetURLByDeviceList(URL,400);
          if URL<>'' then
            begin
              WriteLn(STR_Info+'IP '+URL+' found.');
            end
          else
            begin
              WriteLn(STR_Error+'No IP for "'+(GetOptionValue('u', 'url'))+'" found.');
              Terminate;
              Exit;
            end;
        end;
    end
  else
    begin
      WriteLn(STR_Error+'No URL specified');
      HelpHint;
      Terminate;
      Exit;
    end;

  //read device information
  if Command='DEVICEINFO' then
    begin
      Buffer3:=TStringList.Create;
      Buffer3:=tsapi_Info_DeviceInformation(URL, INT_Timeout);
      if Buffer3.Count=0 then
        begin
          WriteLn(STR_Info,'No device information available!')
        end
      else
        begin
          if Buffer3.Count>0 then
            begin
              WriteLn('');
              WriteLn('                ID                 |               Value               ');
              WriteLn('-----------------------------------|-----------------------------------');
              for Buffer2:=0 to Buffer3.Count-1 do
                begin
                  WriteLn(Format('%0:-35s',[Buffer3.Strings[Buffer2].Split('=')[0]]):35,'|',Format('%0:-35s',[Buffer3.Strings[Buffer2].Split('=')[1]]):35);
                end;
            end;
        end;
      {$IFDEF PROG_DEBUG}
      WriteLn('');
      writeln(STR_Debug,'DEVICEINFO -> RESPONSE BEGIN');
      writeln('--------------------------------------------------------------------------------');
      Writeln(Buffer3.Text);
      writeln('--------------------------------------------------------------------------------');
      writeln(STR_Debug,'DEVICEINFO -> RESPONSE END'+#13#10+#13#10);
      {$ENDIF}
      Terminate;
      Exit;
    end;

  //send keep alive request
  if Command='KEEPALIVE' then
    begin
      if tsapi_Info_KeepAlive(URL, INT_Timeout)=true then
        WriteLn(STR_Info,'Keep-alive request successful!')
      else
        WriteLn(STR_Info,'Keep-alive request failed (possibly device not found/inactive or no authentication)!');
      Terminate;
      Exit;
    end;

  //PIN check
  if HasOption('p', 'pin') then
    begin
      PIN:=(GetOptionValue('p', 'pin'));
    end
  else
    begin
      for Buffer4:=0 to ParamCount do
        begin
          Buffer:=Buffer+ParamStr(Buffer4);
        end;
      //additional PIN check for fullraw command
      if Pos('pin=',LowerCase(Buffer))=0 then
        begin
          WriteLn(STR_Error+'No PIN specified');
          WriteLn(STR_Warning+'Trying default PIN "0000"');
          PIN:='0000';
        end;
    end;
  if TryStrToInt(PIN,Buffer4)=false then
    begin
      WriteLn(STR_Error+'Incorrect PIN. PIN is not strictly numeric (should be 4 numeric digits).');
      WriteLn(STR_Warning+'Trying default PIN "0000"');
      PIN:='0000';
    end;
  if Length(PIN)<>4 then
    begin
      WriteLn(STR_Error+'Incorrect PIN length. PIN has '+IntToStr(Length(PIN))+' digits (should be 4).');
      WriteLn(STR_Warning+'Trying default PIN "0000"');
      PIN:='0000';
    end;
  Buffer:='';
  Buffer4:=0;

  //send zoom request
  if AnsiLeftStr(Command,4)='ZOOM' then
    begin
      Buffer3:=TStringList.Create;
      Buffer3.Text:=Command;
      if Buffer3.Strings[0].Split(':')[1]<>'' then
        begin
          Buffer4:=StrToInt(Buffer3.Strings[0].Split(':')[1]);
          if tsapi_zoomRequest(URL, PIN, Buffer4, INT_Timeout)=true then
            WriteLn(STR_Info,'Zoom request with value "'+IntToStr(Buffer4)+'" should be successful!')
          else
            WriteLn(STR_Info,'Zoom request with value "'+IntToStr(Buffer4)+'" failed (possibly device not found/inactive or wrong PIN)!');
        end
      else
        WriteLn(STR_Info,'Zoom request has no value!');
      Terminate;
      Exit;
    end;

  //send input text request
  if AnsiLeftStr(Command,4)='TEXT' then
    begin
      Buffer3:=TStringList.Create;
      Buffer3.Text:=Command2;
      Buffer:=Buffer3.Strings[0].Split(':')[1];
      if AnsiRightStr(UpperCase(Buffer),3)<>'/CR' then
        begin
          if tsapi_inputTextRequest(URL, PIN, Buffer, false, INT_Timeout)=true then
            WriteLn(STR_Info,'Text input request with "'+Buffer+'" should be successful!')
          else
            WriteLn(STR_Info,'Text input request with "'+Buffer+'" failed (possibly device not found/inactive or wrong PIN)!');
        end
      else
        begin
          Buffer:=AnsiLeftStr(Buffer,Length(Buffer)-3);
          if tsapi_inputTextRequest(URL, PIN, Buffer, true, INT_Timeout)=true then
            WriteLn(STR_Info,'Text input request with "'+Buffer+'" + ENTER should be successful!')
          else
            WriteLn(STR_Info,'Text input request with "'+Buffer+'" + ENTER failed (possibly device not found/inactive or wrong PIN)!');
        end;
      Terminate;
      Exit;
    end;

  //send mouse move request
  if AnsiLeftStr(Command,9)='MOUSEMOVE' then
    begin
      Buffer3:=TStringList.Create;
      Buffer3.Text:=Command;
      if (Buffer3.Strings[0].Split(':')[1]<>'') and (Buffer3.Strings[0].Split(':')[2]<>'') then
        begin
          Buffer4:=StrToInt(Buffer3.Strings[0].Split(':')[1]);
          Buffer5:=StrToInt(Buffer3.Strings[0].Split(':')[2]);
          if tsapi_mouseMoveRequest(URL, PIN, Buffer4, Buffer5, INT_Timeout)=true then
            WriteLn(STR_Info,'Mouse move request with X/Y values "'+IntToStr(Buffer4)+':'+IntToStr(Buffer5)+'" should be successful!')
          else
            WriteLn(STR_Info,'Mouse move request with X/Y values "'+IntToStr(Buffer4)+':'+IntToStr(Buffer5)+'" failed (possibly device not found/inactive or wrong PIN)!');
        end
      else
        WriteLn(STR_Info,'Mouse move request has incomplete X/Y values!');
      Terminate;
      Exit;
    end;

  //send mouse scroll request
  if AnsiLeftStr(Command,11)='MOUSESCROLL' then
    begin
      Buffer3:=TStringList.Create;
      Buffer3.Text:=Command;
      if Buffer3.Strings[0].Split(':')[1]<>'' then
        begin
          Buffer4:=StrToInt(Buffer3.Strings[0].Split(':')[1]);
          if tsapi_mouseScrollRequest(URL, PIN, Buffer4, INT_Timeout)=true then
            WriteLn(STR_Info,'Mouse scroll request with value "'+IntToStr(Buffer4)+'" should be successful!')
          else
            WriteLn(STR_Info,'Mouse scroll request with value "'+IntToStr(Buffer4)+'" failed (possibly device not found/inactive or wrong PIN)!');
        end
      else
        WriteLn(STR_Info,'Mouse scroll request has no value!');
      Terminate;
      Exit;
    end;

  //send mouse button click request
  if AnsiLeftStr(Command,10)='MOUSECLICK' then
    begin
      Buffer3:=TStringList.Create;
      Buffer3.Text:=Command;
      if Buffer3.Strings[0].Split(':')[1]<>'' then
        begin
          if AnsiIndexText(LowerCase(Buffer3.Strings[0].Split(':')[1]), tsapi_MouseButtons)>-1 then
            begin
              if tsapi_mouseClickRequest(URL, PIN, LowerCase(Buffer3.Strings[0].Split(':')[1]), tsapi_MouseState_pressed, INT_Timeout)=true then
                begin
                  if tsapi_mouseClickRequest(URL, PIN, LowerCase(Buffer3.Strings[0].Split(':')[1]), tsapi_MouseState_released, INT_Timeout)=true then
                    WriteLn(STR_Info,'Mouse button click request with value "'+Buffer3.Strings[0].Split(':')[1]+'" should be successful!')
                  else
                    WriteLn(STR_Info,'Mouse button click request with value "'+Buffer3.Strings[0].Split(':')[1]+'" failed!');
                end
              else
                WriteLn(STR_Info,'Mouse button click request with value "'+Buffer3.Strings[0].Split(':')[1]+'" failed (possibly device not found/inactive or wrong PIN)!');
            end
          else
            WriteLn(STR_Info,'Mouse button click request with wrong button value "'+Buffer3.Strings[0].Split(':')[1]+'"!');
        end
      else
        WriteLn(STR_Info,'Mouse button click request has no button value!');
      Terminate;
      Exit;
    end;

  //send authenticate request
  if Command='AUTHENTICATION' then
    begin
      if tsapi_Info_Authentication(URL, PIN, INT_Timeout)=true then
        WriteLn(STR_Info,'Authentication request successful!')
      else
        WriteLn(STR_Info,'Authentication request with PIN "'+PIN+'" failed (possibly device not found/inactive or wrong PIN)!');
      Terminate;
      Exit;
    end;

  //check for existing command
  if HasOption('s', 'sendbutton') then
    begin
      //check ButtonState
      if HasOption('t', 'buttonstate') then
        begin
          ButtonState:=LowerCase(GetOptionValue('t', 'buttonstate'));
          //check if ButtonState is correct
          if AnsiIndexText(ButtonState, tsapi_ButtonStates)<0 then
            begin
              WriteLn(STR_Info,'Button state "'+ButtonState+'" not allowed. ButtonState removed!');
              ButtonState:='';
            end;
        end;

      //check button repeat
      if HasOption('r', 'buttonrepeat') then
        begin
          Buffer:=(GetOptionValue('r', 'buttonrepeat'));
          if TryStrToInt(Buffer, Buffer5)=false then
            begin
              WriteLn(STR_Info,'Button repeat "'+Buffer+'" not allowed. Button repeat removed!');
              Buffer5:=1;
            end;
          if Buffer5<1 then
            begin
              WriteLn(STR_Info,'Negative button repeat "'+IntToStr(Buffer5)+'" not allowed. Button repeat removed!');
              Buffer5:=1;
            end;
        end
      else
        Buffer5:=1;

      //send button
      Command:=UpperCase((GetOptionValue('s', 'sendbutton')));
      Buffer4:=tsapi_BtnCodeByName(Command);
      if Buffer4>=128 then
        begin
          if TryStrToInt(Command,Buffer4)=false then
            begin
              WriteLn(STR_Error+'Incorrect RCU code or button "'+Command+'" specified');
              HelpHint;
              Terminate;
              Exit;
            end;
        end;
      if ButtonState='' then
        begin
          //optionally repeat send button
          for Buffer6:=1 to Buffer5 do
            begin
              if tsapi_rcuButtonRequest(URL, PIN, Buffer4, tsapi_ButtonStates[0], INT_Timeout)=true then
                begin
                  if tsapi_rcuButtonRequest(URL, PIN, Buffer4, tsapi_ButtonStates[1], INT_Timeout)=true then
                    WriteLn(STR_Info,'Sendbutton "'+tsapi_BtnDescByCode(Buffer4)+'" should be successful!')
                  else
                    WriteLn(STR_Info,'Sendbutton "'+tsapi_BtnDescByCode(Buffer4)+'" failed (possibly device not found/inactive or wrong PIN)!');
                end
              else
                WriteLn(STR_Info,'Sendbutton "'+tsapi_BtnDescByCode(Buffer4)+'" failed (possibly device not found/inactive or wrong PIN)!');
              //in case of sending multiple times do a pause
              if Buffer5>1 then
                Sleep(100);
            end;
        end
      else
        begin
          for Buffer6:=1 to Buffer5 do
            begin
              if tsapi_rcuButtonRequest(URL, PIN, Buffer4, ButtonState, INT_Timeout)=true then
                WriteLn(STR_Info,'Sendbutton "'+tsapi_BtnDescByCode(Buffer4)+'" with "'+ButtonState+'" should be successful!')
              else
                WriteLn(STR_Info,'Sendbutton "'+tsapi_BtnDescByCode(Buffer4)+'" with "'+ButtonState+'" failed (possibly device not found/inactive or wrong PIN)!');
              //in case of sending multiple times do a pause
              if Buffer5>1 then
                Sleep(100);
            end;
        end;
    end
  else
    begin
      if not (HasOption('o', 'loop')) then
        begin
          WriteLn(STR_Error+'No RCU code or button specified');
          HelpHint;
          Terminate;
          Exit;
        end;
    end;

  //start remote loop
  if HasOption('o', 'loop') then
    begin
      writeln(STR_Info+'Loop entered. Press a key for a remote action. Leave loop with [CTRL+C] or [ESC].');
      while true do
        begin
          InitKeyboard;
          if KeyPressed then
            begin
              Buffer7:=PollKeyEvent;
              {$IFDEF PROG_DEBUG}
              writeln(STR_Debug+KeyEventToString(Buffer7));
              {$ENDIF}
              case GetKeyEventCode(Buffer7) of
                283:    begin writeln(STR_Info+'[ESC] Leaving loop.'); break; end;
                561:    begin ButtonRequest(URL, PIN, 1, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(1)); end;   //1
                818:    begin ButtonRequest(URL, PIN, 2, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(2)); end;   //2
                1075:   begin ButtonRequest(URL, PIN, 3, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(3)); end;   //3
                1332:   begin ButtonRequest(URL, PIN, 4, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(4)); end;   //4
                1589:   begin ButtonRequest(URL, PIN, 5, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(5)); end;   //5
                1846:   begin ButtonRequest(URL, PIN, 6, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(6)); end;   //6
                2103:   begin ButtonRequest(URL, PIN, 7, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(7)); end;   //7
                2360:   begin ButtonRequest(URL, PIN, 8, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(8)); end;   //8
                2617:   begin ButtonRequest(URL, PIN, 9, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(9)); end;   //9
                2864:   begin ButtonRequest(URL, PIN, 0, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(0)); end;   //0
                20736:  begin ButtonRequest(URL, PIN, 64, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(64)); end; //page down
                18688:  begin ButtonRequest(URL, PIN, 63, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(63)); end; //page up
                20480:  begin ButtonRequest(URL, PIN, 31, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(31)); end; //down
                18432:  begin ButtonRequest(URL, PIN, 30, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(30)); end; //up
                19200:  begin ButtonRequest(URL, PIN, 34, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(34)); end; //left
                19712:  begin ButtonRequest(URL, PIN, 35, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(35)); end; //right
                7181:   begin ButtonRequest(URL, PIN, 36, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(36)); end; //ok
                12654:  begin ButtonRequest(URL, PIN, 62, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(62)); end; //nav
                12909:  begin ButtonRequest(URL, PIN, 32, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(32)); end; //menu
                11386:  begin ButtonRequest(URL, PIN, 45, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(45)); end; //zoom
                6512:   begin ButtonRequest(URL, PIN, 44, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(44)); end; //pip/pap
                9064:   begin ButtonRequest(URL, PIN, 47, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(47)); end; //hdmi
                6255:   begin ButtonRequest(URL, PIN, 41, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(41)); end; //option
                5236:   begin ButtonRequest(URL, PIN, 25, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(25)); end; //teletext
                12386:  begin ButtonRequest(URL, PIN, 20, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(20)); end; //back
                5993:   begin ButtonRequest(URL, PIN, 29, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(29)); end; //info
                4709:   begin ButtonRequest(URL, PIN, 23, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(23)); end; //epg
                9836:   begin ButtonRequest(URL, PIN, 33, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(33)); end; //tv/radio
                8051:   begin ButtonRequest(URL, PIN, 11, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(11)); end; //standby
                12150:  begin ButtonRequest(URL, PIN, 12, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(12)); end; //mute
                7777:   begin ButtonRequest(URL, PIN, 21, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(21)); end; //audio
                11640:  begin ButtonRequest(URL, PIN, 67, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(67)); end; //timer
                5497:   begin ButtonRequest(URL, PIN, 22, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(22)); end; //still
                20992:  begin ButtonRequest(URL, PIN, 18, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(18)); end; //prog+
                21248:  begin ButtonRequest(URL, PIN, 19, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(19)); end; //prog-
                15104:  begin ButtonRequest(URL, PIN, 37, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(37)); end; //red
                15360:  begin ButtonRequest(URL, PIN, 38, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(38)); end; //green
                15616:  begin ButtonRequest(URL, PIN, 39, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(39)); end; //yellow
                15872:  begin ButtonRequest(URL, PIN, 40, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(40)); end; //blue
                16128:  begin ButtonRequest(URL, PIN, 49, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(49)); end; //rewind
                16384:  begin ButtonRequest(URL, PIN, 50, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(50)); end; //stop
                16640:  begin ButtonRequest(URL, PIN, 51, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(51)); end; //play/pause
                16896:  begin ButtonRequest(URL, PIN, 52, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(52)); end; //forward
                4978:   begin ButtonRequest(URL, PIN, 43, INT_Timeout); writeln(STR_Info+tsapi_BtnDescByCode(43)); end; //record
                else writeln(STR_Info+'Unknown or not supported key '+IntToStr(GetKeyEventCode(Buffer7)));
              end;
              DoneKeyboard;
            end;
        end;
    end;
  // stop program loop
  Terminate;
end;

constructor TApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TApp.Destroy;
begin
  inherited Destroy;
end;

procedure TApp.WriteHelp;
begin
//Long help
  WriteLn('General usage:          ', ExtractFileName(ExeName), ' --url=[IP or LOCAL DOMAIN] --pin=[DEVICE PIN] --sendbutton=[CODE or NAME]');
  WriteLn('                        or');
  WriteLn('                        ', ExtractFileName(ExeName), ' -u [IP or LOCAL DOMAIN] -p [DEVICE PIN] -s [CODE or NAME]');
  WriteLn('                        or');
  WriteLn('                        ', ExtractFileName(ExeName), ' --url=[IP or LOCAL DOMAIN] --pin=[DEVICE PIN] --command=[COMMAND]');
  WriteLn('                        or');
  WriteLn('                        ', ExtractFileName(ExeName), ' -u [IP or LOCAL DOMAIN] -p [DEVICE PIN] -c [COMMAND]');
  WriteLn('                        or');
  WriteLn('                        ', ExtractFileName(ExeName), ' --url=[IP or LOCAL DOMAIN] --pin=[DEVICE PIN] --command=[COMMAND]:[+/-VALUE]');
  WriteLn('                        or');
  WriteLn('                        ', ExtractFileName(ExeName), ' -u [IP or LOCAL DOMAIN] -p [DEVICE PIN] -c [COMMAND]:[+/-VALUE]');
  WriteLn('                        or');
  WriteLn('                        ', ExtractFileName(ExeName), ' --url=[IP or LOCAL DOMAIN] --pin=[DEVICE PIN] --loop');
  WriteLn('                        or');
  WriteLn('                        ', ExtractFileName(ExeName), ' -u [IP or LOCAL DOMAIN] -p [DEVICE PIN] -o');
  WriteLn('');
  WriteLn('General usage examples: ', ExtractFileName(ExeName), ' --url=192.168.0.34 --pin=1234 --sendbutton=BTN_OK');
  WriteLn('                        ', ExtractFileName(ExeName), ' --url=192.168.0.34 --pin=1234 --command=DEVICEINFO');
  WriteLn('                        ', ExtractFileName(ExeName), ' --url="@0008abcdef123456" -p 1234 -s 12');
  WriteLn('                        ', ExtractFileName(ExeName), ' -u TECHNIVISTA-SL.fritz.box -p 9999 -c keepalive');
  WriteLn('                        ', ExtractFileName(ExeName), ' -u "@TECHNIBOX UHD S" -p 1234 -s 12');
  WriteLn('                        ', ExtractFileName(ExeName), ' --url=192.168.0.34 -p 1234 -s BTN_1 --buttonstate="pressed" --buttonrepeat=2');
  WriteLn('                        ', ExtractFileName(ExeName), ' --url=192.168.0.34 -p 1234 -o');
  WriteLn('                        ', ExtractFileName(ExeName), ' --url=192.168.0.34 -p 1234 --command="zoom:-2"');
  WriteLn('                        ', ExtractFileName(ExeName), ' --url=192.168.0.34 -p 1234 --command="mousemove:-15:10"');
  WriteLn('                        ', ExtractFileName(ExeName), ' --url=192.168.0.34 -p 1234 --command="mouseclick:left"');
  WriteLn('');
  WriteLn('Usage hints:            Values with one or more spaces the value must be quoted with " (for e.g. "@DEVICENAME").');
  WriteLn('                        Some commands need additional values. These values are added with ":" after the command.');
  WriteLn('                        The PIN will only be used if a authentication is needed. So if the device is already');
  WriteLn('                        authenticated, no or also a wrong PIN will work, too, as long as the authentication');
  WriteLn('                        must not be renewed.');
  WriteLn('');
  WriteLn('List of commands for use with -c (--command):');
  WriteLn('AUTHENTICATION          Starts Authentication process. URL and PIN are also required.');
  WriteLn('DEVICEINFO              Read device info (authenticate first, no PIN needed).');
  WriteLn('KEEPALIVE               Send a keep-alive request (authenticate first, no PIN needed).');
  WriteLn('ZOOM:                   Send a zoom request in browser mode.');
  WriteLn('                        ZOOM IN with a positive value, for e.g. 1, and ZOOM OUT with a negative value, for e.g. -1.');
  WriteLn('MOUSEMOVE:              Send a mouse move request in browser mode.');
  WriteLn('                        This needs 2 values for X and Y axis. Positive values go right/down, negative values left/up.');
  WriteLn('                        Example: MOUSEMOVE:-10:20');
  WriteLn('MOUSECLICK:             Send a mouse click request in browser mode.');
  WriteLn('                        This needs a button value left, middle or right.');
  WriteLn('TEXT:                   Send a text when virtual keyboard is open. Add /CR and the end of the text for sending an ENTER.');
  WriteLn('');
  WriteLn('Program functions:');
  WriteLn('Special commands:  ', ExtractFileName(ExeName), ' -c (--command)');
  WriteLn('                   Send a special command.'+#13#10);
  WriteLn('Device list:       ', ExtractFileName(ExeName), ' -d (--devicelist)');
  WriteLn('                   Broadcast network scan of supported active devices if available.');
  WriteLn('                   This scan may take up around to 5 seconds.'+#13#10);
  WriteLn('Target device URL: ', ExtractFileName(ExeName), ' -u (--url)');
  WriteLn('                   This defines the target device URL adress. This might be a local IP or domain.');
  WriteLn('                   Optionally the device name or serial can be used with a preceding @, for e.g. -u "@DIGIPLUS UHD S".');
  WriteLn('                   The serial should start with 0008xxxxxxxxxxxx.');
  WriteLn('                   Name and serial can be found in device menues like "System configuration" or can be identified');
  WriteLn('                   by using the -d (--devicelist) function.');
  WriteLn('                   The device name or serial must be quoted with ", for e.g. -u "@0008abcdef123456".');
  WriteLn('                   Using the device name is not recommended if more devices are using the same name within the ');
  WriteLn('                   same network. In this case the IP or device serial should be preferred over the name.');
  WriteLn('                   Using the name or serial instead of the IP slows down the RCU sendbutton process a little bit.'+#13#10);
  WriteLn('PIN:               ', ExtractFileName(ExeName), ' -p (--pin)');
  WriteLn('                   PIN for the authentication. The PIN can only be set in the target device.'+#13#10);
  WriteLn('Send RCU button:   ', ExtractFileName(ExeName), ' -s (--sendbutton)');
  WriteLn('                   Send a RCU button. URL and PIN are also required. See possible buttons with -r (--rcucommands).'+#13#10);
  WriteLn('RCU command list:  ', ExtractFileName(ExeName), ' -x (--rcucommands)');
  WriteLn('                   Show all available RCU commands with code, name and description.');
  WriteLn('                   Button names start with "BTN_" but for compability issues also "KEY_" is allowed.');
  WriteLn('                   For e.g. "KEY_OK" can be used as equal replacement for "BTN_OK".'+#13#10);
  WriteLn('Use button state:  ', ExtractFileName(ExeName), ' -t (--buttonstate)');
  WriteLn('                   Optionally defines a state for the to send RCU button.');
  WriteLn('                   Allowed states are "pressed", "released" and "hold".');
  WriteLn('                   Depending on the send RCU button the sendbutton command must be used twice');
  WriteLn('                   one time with "pressed" and again with "released".'+#13#10);
  WriteLn('Use button repeat: ', ExtractFileName(ExeName), ' -r (--buttonrepeat)');
  WriteLn('                   Optionally defines how often to send the same RCU button. Default is one time.'+#13#10);
  WriteLn('Loop:              ', ExtractFileName(ExeName), ' -o (--loop)');
  WriteLn('                   Enter loop mode. URL and PIN are also required.');
  WriteLn('                   In loop mode it is possible to send continuously buttons to the target device via keyboard keys.');
  WriteLn('                   The programm window must be in foreground/active to catch key presses.'+#13#10);
  WriteLn('Loop command list: ', ExtractFileName(ExeName), ' -y (--loopcommands)');
  WriteLn('                   Show all available loop commands with with the mapped keyboard key.'+#13#10);
  WriteLn('Help:              ', ExtractFileName(ExeName), ' -h (--help)');
  WriteLn('                   Show this help text.'+#13#10);
  WriteLn('Build info:        ', ExtractFileName(ExeName), ' -b (--build)');
  WriteLn('                   Show the program build info.'+#13#10);
  WriteLn('API info:          ', ExtractFileName(ExeName), ' -a (--api)');
  WriteLn('                   Show the Frontier Silicon API info.'+#13#10);
  WriteLn('Banner:            ', ExtractFileName(ExeName), ' -n (--nobanner)');
  WriteLn('                   Hide the banner.'+#13#10);
  WriteLn('                   ', ExtractFileName(ExeName), ' -i (--showbanner)');
  WriteLn('                   Just show the banner (overrides -n --nobanner).'+#13#10);
  WriteLn('License info:      ', ExtractFileName(ExeName), ' -l (--license)');
  WriteLn('                   Show license info.'+#13#10);
end;

procedure TApp.HelpHint;
//show a hint for the help function
begin
  WriteLn(STR_Info+'Try "', ExtractFileName(ExeName), ' -h" or "', ExtractFileName(ExeName), ' --help" for a detailed help.');
  WriteLn(STR_Info+'Try "', ExtractFileName(ExeName), ' -l" or "', ExtractFileName(ExeName), ' --license" for the license.');
end;

procedure TApp.WaitPrint;
//show waiting hint
begin
  Write(STR_Info+STR_WaitingMsg);
end;

procedure TApp.WaitClear;
//clear waiting hint
begin
  Write(StringOfChar(#8, Length(STR_Info)+Length(STR_WaitingMsg))+StringOfChar(' ', Length(STR_Info)+Length(STR_WaitingMsg)));
end;

procedure TApp.ButtonRequest(URL: String; PIN: String; Button: Byte; INT_Timeout: Integer);
//Simple sendbutton procedure without result check
begin
  tsapi_rcuButtonRequest(URL, PIN, Button, tsapi_ButtonStates[0], INT_Timeout);
  tsapi_rcuButtonRequest(URL, PIN, Button, tsapi_ButtonStates[1], INT_Timeout);
end;

var
  Application: TApp;
begin
  Application:=TApp.Create(nil);
  Application.Run;
  Application.Free;
end.

