program hSplit;

(* hSplit
**
** Syntax
** hSplit.exe [Param1 [Value1 [Param2 Value2 ... ...]]] "Source.file" ["..."]
**
** Switches
** -h > The Help
**
** -S > Segments: split file into segments (default is split Chunks)
**      An array of comma separated start and end bytes is used
**      values for start and end can be an Integer or a Hex string
**      to use hex values set a first char to "h" -> h0-1000,2000-4000
**      if an end value is set 0 means end of file(last byte)
**      the end byte is NOT included in the target file
**      a byte pair of 0-0 means copy the entire file and will ne skipped
**      the start byte must be smaller than the end byte
**
** -s > Chunk size: value is an Integer and default means Megabyte (def=20)
**      a first or last char can be set to define the value type
**      b=Byte; k=Kilobyte g=Gigabyte
**
** -c > Chunks count: How many chunks should be written
**      if not set or set to 0 means infinity until the source file ends (def=0)
**
** -t > Target folder: if not set or empty the source folder is used
**
** -n > Target file name: if not set or empty the source filename is used
**
** -P > Progress interval: Control how often the progress is send to the output
**      Value is a Byte from 0 to 255
**      Value 0 sends no progress numbers; 1=max 255=min (def=100)
**
** -J > Join files: value is a path for a target file
**      only the -P switch can be used additionally
**
** ".." > Source file(s): MUST be the last parameter(s)! Path to source file(s)
**
** @".." > Path to a parameter file: MUST be the first parameter!
**         The parameter file should be a simple text file
**         You can use any name and extension for the file name
**         Only source file parameters can be used additionally
**
** Parameter file
** Each line is used as a parameter
** If you don't set a source path in the parameter file,
** the source path(s) is(are) read from the commandline.
** You can use a root parameter file called "params.txt"
** Exists this file in the root folder of hSplit, it will be used automatically.
** If the "params.txt" is empty, it will be ignored.
**
** Command
** You can send a command while hSplit is running.
** Command will be executed after pressing the ENTER key
**
** "e" > User Exit: stops hSplit
**
** Exit codes
** hSplit sends at the end an exit code

** 0 - all OK
** 1 - no source file
** 2 - error while loading source file
** 3 - no target folder (not found or could not created)
** 4 - chunk size too big (bigger than the source file size)
** 5 - nothing to split (no data)
** 6 - nothing to join (no data)
** 7 - parameter file not found
** 8 - parameter file is empty (only if used with the cli parameter)
** 9 - wrong parameters
** 99 - internal Lazarus error
** -1 - User Exit


*)

{$mode objfpc}{$H+}

uses
 // Original
 //{$IFDEF UNIX}{$IFDEF UseCThreads}
 //cthreads,
 //{$ENDIF}{$ENDIF}
 {$ifdef unix}
 cthreads,
 cmem,// the c memory manager is on some systems much faster for multi-threading
 {$endif}
 Classes, SysUtils, CustApp, RegExpr,
 ConsoleRead, hSplitThread;

// Compiler switches
//{$define debug}

const
  hSplitTitle           = 'hSplit rev(0.02)';
  Interval_Factor       = 1024;
  hsParams_txt          = 'params.txt';

type


 { THSplit }

 THSplit                = class(TCustomApplication)
 private                // vars
  FhSplitThread         : ThSplitThread;
  FProgress_Interval    : Integer;
  FProgress_IntervalCounter: Integer;

  FErrorText            : String;

  FPara_SplitSegments   : Boolean;               // Segmente splitten
  FPara_ChunkSize       : String;
  FPara_ChunkCount      : String;
  FPara_SegSplitArray   : String;
  FPara_ProgressInterval: String;                // 0=no 1=max 255=min def=100
  FPara_TrgFolder       : String;
  FPara_TrgFilename     : String;
  FPara_Source          : String;
  FJoinFiles            : TStringList;           // Datei Pfade

 private                // Prozeduren
  function Check_Params: Boolean;
  function Check_HelpParam: Boolean;

  function Parse_Params: Boolean;
  function Start_hSplit: Boolean;

  procedure Log_ProgressNums;
  procedure Log_End;
  procedure DoUserExit;

 protected
  procedure DoRun; override;
 public
  constructor Create(TheOwner: TComponent); override;
  destructor Destroy; override;
  procedure WriteHelp; virtual;
 end;

{ THSplit }


(*------------ private -------------------------------------------------------*)


// Parameter Check
function THSplit.Check_Params: Boolean;
var
  errMsg: String;
begin
 Result:=false;
 (* Keine Params *)
 if ParamCount = 0 then                          // Keine Params
 begin
  WriteHelp;                                     // Hilfe anzeigen
  Exit;                                          // fertig
 end;

 errMsg:=CheckOptions('hSsctnPJ', '',true);
 if errMsg <> '' then
 begin
  ExitCode:=hsErr_WrongParams;                   // Exitcode setzen
  ShowException(Exception.Create(errMsg));
  //ReadLn;
  Exit;
 end;
 Result:=true;
end;


// Hilfe Parameter Check
function THSplit.Check_HelpParam: Boolean;
begin          
  Result:=HasOption('h', '');
  if Result then WriteHelp;
end;


// Parameter parsen
function THSplit.Parse_Params: Boolean;
var
  p: Integer;
  param,paramValue: String;
  doContinue: Boolean=false;
  Params_txtIsUsed: Boolean=false;


  // Param Value prüfen
  function Check_ParamValue: Boolean;
  var
    regex :TRegExpr=nil;
    expression,msg: String;
  begin
    Result:=false;
    case param of
     (* Segments *)
     '-S':
     begin
      if pos (',',paramValue) > 0 then
      expression:='[h]?[\da-fA-F]+[\-][\da-fA-F]+([\,][\da-fA-F]+[\-][\da-fA-F]+)+'
      else
      expression:='[h]?[\da-fA-F]+[\-][\da-fA-F]+';
      msg:='Error parameter '+inttostr(p+1)
      +': incorrect format of the split array'
      +#13#10'usage: (h)start-end,start-end,...';
     end;
     (* Chunk size *)
     '-s':
     begin
      expression:='^\d+$|^[bkg]\d+$|^\d+[bkg]$';
      msg:='Error parameter '+inttostr(p+1)
      +': incorrect format of the chuck size'
      +#13#10'usage: 123 or b456 or k789 or g1 or 456b or 789k or 1g';
     end;
     (* Chunk count *)
     '-c':
     begin
      expression:='\d+';
      msg:='Error parameter '+inttostr(p+1)
      +': incorrect format of the chuck count'
      +#13#10'usage: an Integer 123';
     end;
     (* Progress Interval *)
     '-P':
     begin
      expression:='\d+';
      msg:='Error parameter '+inttostr(p+1)
      +': incorrect format of the progress interval'
      +#13#10'usage: an Integer from 0 to 255';
     end;

    end;

    (* Prüfen *)
    regex:=TRegExpr.Create(expression);
    Result:=regex.Exec(paramValue);              // Value String prüfen OK
    FreeAndNil(regex);
    if not Result then                           // NICHT OK
    begin
     WriteLn(msg);                               // Message ausgeben
     ReadLn;
    end;
  end;


  // Param parsen
  function Parse_Param: Boolean;
  begin
    Result:=false;
    case param of                                // Param auswerten
     '-S':                                       // Segments
     begin
      FPara_SplitSegments:=true;                 // Segmente splitten
      if not Check_ParamValue then Exit;         // Param Value prüfen
      FPara_SegSplitArray:=paramValue;           // paramValue sichern
      doContinue:=true;                          // nächsten Param überspringen
     end;

     '-s':                                       // Chunk size
     begin
      if not Check_ParamValue then Exit;         // Param Value prüfen
      FPara_ChunkSize:=paramValue;               // paramValue sichern
      doContinue:=true;                          // nächsten Param überspringen
     end;

     '-c':                                       // Chunk count
     begin
      if not Check_ParamValue then Exit;         // Param Value prüfen
      FPara_ChunkCount:=paramValue;              // paramValue sichern
      doContinue:=true;                          // nächsten Param überspringen
     end;

     '-t':                                       // Target Folder
     begin
      FPara_TrgFolder:=paramValue;               // paramValue sichern
      doContinue:=true;                          // nächsten Param überspringen
     end;

     '-n':                                       // Target file name
     begin
      FPara_TrgFilename:=paramValue;             // paramValue sichern
      doContinue:=true;                          // nächsten Param überspringen
     end;

     '-P':                                       // Progress interval
     begin
      if not Check_ParamValue then Exit;         // Param Value prüfen
      FPara_ProgressInterval:=paramValue;        // paramValue sichern
      doContinue:=true;                          // nächsten Param überspringen
     end;

     '-J':                                       // Join files
     begin
      (* Ziel Pfad *)
      FPara_TrgFilename:=paramValue;             // paramValue sichern
      FPara_TrgFolder:=ExtractFileDir(paramValue);// Ziel Ordner sichern
      FJoinFiles:=TStringList.Create;            // Join File List erzeugen
      doContinue:=true;                          // nächsten Param überspringen
     end;

    end;

    (* Join Pfade *)
    if FJoinFiles <> nil then                    // Join List vorhanden
    FJoinFiles.Add(param);                       // Datei pfad aufnehmen
    Result:=true;
  end;


  // Param.txt prüfen
  function Check_Params_txt: Boolean;
  var
    s,s_Count: integer;
    params_txt_by_param1: Boolean=false;
    FParams_txt            : TStringList;
    FParams_txtPath        : String;

  begin
    Result:=false;
    (* Params.txt ist erster cli Param *)
    param:=Params[1];                            // 1.Param ermitteln
    if param[1] = '@' then                       // @ Zeichen gefunden
    begin
     params_txt_by_param1:=true;                 // params file im 1.param
     FParams_txtPath:=copy(param,2,Length(param));// Pfad ermitteln
    end
    else                                         // Params.txt im Root suchen
    FParams_txtPath:=ExtractFileDir(ExeName)     // Pfad im Root ordner
    +DirectorySeparator+hsParams_txt;

    if not FileExists(FParams_txtPath) then      // Datei nicht da
    begin
     if params_txt_by_param1 then                // erster Param wurde benutzt
     begin
      ExitCode:=hsErr_NoParams_txt;              // Params Datei nicht gefunden
      WriteLn('The params file could not be found.');
      Exit;
     end;
     // Param Datei check erfolgreich aber nicht benutzt
     Result:=true;                               // Result auf true
     Exit;
    end;

    (* Params Datei verarbeiten *)
    FParams_txt:=TStringList.Create;             // Params Liste erzeugen
    FParams_txt.LoadFromFile(FParams_txtPath);   // Liste laden

    s_Count:=FParams_txt.Count;                  // Anzahl strings ermitteln

    (* Leere Params Datei - nur bei 1.cli param *)
    if s_Count = 0 then                          // Datei ist leer
    begin
     FParams_txt.Free;                           // Liste freigeben
     if params_txt_by_param1 then                // Params Datei bei param1
     begin
      ExitCode:=hsErr_Params_txtEmpty;           // Params Datei leer
      WriteLn('The params file is empty.');
     end
     else                                        // Root Param Datei
     // Param Datei check erfolgreich aber nicht benutzt
     Result:=true;                               // Result auf true
     Exit;                                       // Abbruch
    end;

    for s:=0 to s_Count -1 do                    // Liste durchgehen
    begin
     if doContinue then                          // Weiter wenn nötig
     begin
      doContinue:=false;                         // Continue beenden
      param:='';                                 // Param leeren
      Continue;                                  // weiter
     end;

     param:=FParams_txt[s];                      // Parmater String ermitteln
     if s < s_Count-1 then                       // maximal vorletzter param
     paramValue:=FParams_txt[s+1]                // nächster param ist value
     else paramValue:='';                        // ansonst paramValue leeren

     if not Parse_Param then                     // Param parsen erfolgreich??
     begin
      FParams_txt.Free;                          // Liste freigeben
      Exit;                                      // Abbruch
     end;
    end;

    (* Last Param = Source *)
    if FJoinFiles = nil then                     // nicht fürs Joinen
    begin
     FPara_Source:=param;                        // letzten param sichern
     if param = '' then                          // keine Source Datei
     // der letzte Param aus den cli commands wird genutzt
     if ParamCount > 0 then                      // mindestens ein Param
     FPara_Source:=Params[ParamCount];           // letzter cli Param
    end
    else
    // Join Liste prüfen
    begin
     if FJoinFiles.Count = 0 then                // Keine Source Dateien
     if params_txt_by_param1 then                // Param_txt param1
     s:=2 else s:=1;
     for s:=s to ParamCount do                   // Source Params durchgehen
     FJoinFiles.Add(Params[s]);                  // Pfade sichern
    end;

    Params_txtIsUsed:=true;                      // Param Datei wurde benutzt
    Result:=true;
  end;


begin
  Result:=false;
  (* Params Datei *)
  if not Check_Params_txt then Exit;             // Params Datei prüfen erfolg??
  if Params_txtIsUsed then                       // Params Datei wurde verwendet
  begin
   Result:=true;                                 // Result auf true
   Exit;                                         // fertig
  end;
  if ExitCode > 0 then Exit;                     // Abbruch ExitCode größer 0

  for p:=1 to ParamCount do                      // cli Params durchgehen
  begin
   if doContinue then                            // Weiter wenn nötig
   begin
    doContinue:=false;                           // Continue beenden
    param:='';                                   // Param leeren
    Continue;                                    // weiter
   end;

   param:=Params[p];                             // Parmater String ermitteln

   if p < ParamCount then                        // maximal vorletzter param
   paramValue:=Params[p+1]                       // nächster param ist value
   else paramValue:='';                          // ansonst paramValue leeren

   if not Parse_Param then Exit;                 // Param parsen
  end;

  (* Last Param = Source *)
  if FJoinFiles = nil then                       // nicht fürs Joinen
  FPara_Source:=param;                           // letzten param sichern
  Result:=true;
end;


// Start hSplit
function THSplit.Start_hSplit: Boolean;
var
  sBytes,eBytes: QWordArray;
  interval: Integer;
  info: String;


  // Segment split Bytes verarbeiten
  procedure Process_SegmentSplitBytes;
  var
    regex: TRegExpr;
    s,e: TStringList;
    b: Integer;
  begin
    s:=TStringList.Create; e:=TStringList.Create;
    regex:=TRegExpr.Create('([\da-fA-F]+)-([\da-fA-F]+)');
    if regex.Exec(FPara_SegSplitArray) then      // regex hat was gefunden
    repeat
     s.Add(regex.Match[1]);                      // 1.Match = Start
     e.Add(regex.Match[2]);                      // 2.Match = End
    until not regex.ExecNext;                    // Kein Next

    SetLength(sBytes,s.Count);                   // Start array einstellen
    SetLength(eBytes,e.Count);                   // End array einstellen

    if FPara_SegSplitArray[1] = 'h' then         // hex Zahlen
    for b:=0 to s.Count -1 do                    // Anzahl Splits verarbeiten
    begin
     sBytes[b]:=StrToQWord('$'+s[b]);            // Start String zu QWord
     eBytes[b]:=StrToQWord('$'+e[b]);            // End String zu QWord
    end
    else                                         // Normale Zahlen
    for b:=0 to s.Count -1 do                    // Anzahl Splits verarbeiten
    begin
     sBytes[b]:=StrToQWord(s[b]);                // Start String zu QWord
     eBytes[b]:=StrToQWord(e[b]);                // End String zu QWord
    end;
    s.Free; e.Free; regex.Free;
  end;

  // Chunk size verarbeiten
  procedure Process_ChunkSize;
  const kb = 1024; mb = 1024*1024; gb = 1024*1024*1024;
  var
    c_size,c: String;
    q_size: QWord;
    l: Integer;
  begin
    l:=Length(FPara_ChunkSize);                  // Länge ermitteln
    c:=FPara_ChunkSize[1];                       // erstes Zeichen
    (* Type - 1.Pos *)
    if pos(c,'bkg') > 0 then                     // Typen Definition 1.Stelle
    begin
     c_size:=copy(FPara_ChunkSize,2,l);          // Type-Zeichen abtrennen
     q_size:=strtoint(c_size);                   // String in Zahl umwandeln
     case c of                                   // Erstes Zeichen auswerten
      'b': FhSplitThread.ChunkSize:=q_size;      // Byte Angabe
      'k': FhSplitThread.ChunkSize:=q_size*kb;   // Kilo Byte Angabe
      'g': FhSplitThread.ChunkSize:=q_size*gb;   // Giga Byte Angabe
     end;
     Exit;
    end;
    (* Type - Letzte Pos *)
    c:=FPara_ChunkSize[l];                       // letztes Zeichen
    if pos(c,'bkg') > 0 then                     // Typen Definition 1.Stelle
    begin
     c_size:=copy(FPara_ChunkSize,1,l-1);        // Type-Zeichen abtrennen
     q_size:=strtoint(c_size);                   // String in Zahl umwandeln
     case c of                                   // Erstes Zeichen auswerten
      'b': FhSplitThread.ChunkSize:=q_size;      // Byte Angabe
      'k': FhSplitThread.ChunkSize:=q_size*kb;   // Kilo Byte Angabe
      'g': FhSplitThread.ChunkSize:=q_size*gb;   // Giga Byte Angabe
     end;
    end
    else
    if pos(FPara_ChunkSize[1],'bkg') = 0 then    // Keine Typen Definition
    begin
     q_size:=strtoint(FPara_ChunkSize);          // String in Zahl umwandeln
     FhSplitThread.ChunkSize:=q_size*mb;         // Chunk Size setzen > MegaByte
    end;
  end;

begin
  Result:=false;       
  (* Progress Interval *)
  if FPara_ProgressInterval <> '' then           // Interval vorhanden
  begin
   interval:=strtoint(FPara_ProgressInterval);   // Integer erzeugen
   if interval > 255 then interval:=255;         // größer 255 prüfen
   FProgress_Interval:=interval*Interval_Factor; // Interval Faktor anwenden
   FProgress_IntervalCounter:=FProgress_Interval;// Interval Counter einstellen
  end;

  (* Split Thread erzeugen *)
  FhSplitThread:=ThSplitThread.Create;           // Split Thread erzeugen
  FhSplitThread.Source:=FPara_Source;            // Source setzen
  FhSplitThread.TargetFolder:=FPara_TrgFolder;   // Target Folder setzen
  FhSplitThread.TargetFilename:=FPara_TrgFilename;// Target file name setzen

  (* Join *)
  if FJoinFiles <> nil then                      // Join Liste vorhanden
  begin
   info:='join files';                           // Info - Dateien joinen
   FhSplitThread.Insert_JoinFiles(FJoinFiles);   // Join Dateien übertragen
  end
  else

  (* Segments *)
  if FPara_SplitSegments then                    // Segmente splitten
  begin
   info:='split segments';                       // Info - split segments
   FhSplitThread.SegmentSplit:=true;             // Segmente splitten true
   Process_SegmentSplitBytes;                    // Split Bytes verarbeiten
   FhSplitThread.Insert_SegmentSplits(sBytes,eBytes); // Split Bytes einfügen
  end
  else
  (* Chunks *)
  begin
   info:='split chunks';                         // Info - split chunks
   if FPara_ChunkSize <> '' then                 // Chunk size vorhanden
   Process_ChunkSize;                            // Chunk Size verarbeiten
   if FPara_ChunkCount <> '' then                // Chunk Count vorhanden
   FhSplitThread.ChunkCount:=strtoint(FPara_ChunkCount); // Chunk Count setzen
  end;
  (* Thread Start *)
  FhSplitThread.Start;                           // Thread starten
  WriteLn('hSplit Start - '+info);               // start ausgeben
  Result:=true;
end;


// Log - Progress Nums
procedure THSplit.Log_ProgressNums;
var
 total,chunk: Single;
 t,c: String;
begin
  if ExitCode > 0 then Exit;                     // Abbruch Fehler Code > 0
  if FhSplitThread.Finished then                 // Thread ist bereits fertig
  begin
   total:=100; chunk:=100;                       // auf 100 setzen
  end
  else
  begin
   total:=FhSplitThread.ProgressNumTotal;        // Total Progress Nummer
   if total = 100 then                           // wenn 100
   chunk:=100                                    // dann chunk auch 100
   else
   chunk:=FhSplitThread.ProgressNumChunk;        // Chunk Progress Nummer
  end;

  (* Progress Nummern *)
  if (FProgress_Interval = FProgress_IntervalCounter)// Interval ist erreicht
  or (total = 100) then                          // oder Total ist 100
  begin
   if total = 100 then                           // total ist 100
   t:='100'                                      // nur 100 als Wert
   else t:=FormatFloat('0.0',total);             // Zahl formatieren
   if chunk = 100 then                           // chunk ist 100
   c:='100'                                      // nur 100 als Wert
   else c:=FormatFloat('0.0',chunk);             // Zahl formatieren

   WriteLn('Total-Progress: '+t+'% <> '          // Total-Progress
   +'Chunk-Progress: '+c+'%');                   // Chunk-Progress
   FProgress_IntervalCounter:=Interval_Factor;   // Interval Zähler zurücksetzen
  end
  else Inc(FProgress_IntervalCounter);           // Interval Zähler erhöhen
end;


// Log - Ende
procedure THSplit.Log_End;
var
 msg: String;
begin
  msg:=#10#13'Exit Code: '+inttostr(ExitCode);   // msg - Exit Code einfügen
  if FErrorText <> '' then                       // Fehler Text vorhanden
  msg:=msg+'Error Text:'#13#10+FErrorText;       // Fehler Text anhängen
  msg:=msg+#10#13'hSplit done!';                 // hSplit fertig anhängen
  WriteLn(msg);                                  // msg ausgeben
end;


// User Exit
procedure THSplit.DoUserExit;
begin
  FhSplitThread.UserExit:=true;
  ExitCode:=-1;
end;



(*------------ protected -----------------------------------------------------*)


// Do Run
procedure THSplit.DoRun;
var
  user_input: String='';
  command: String='';
begin
  (* Parameter Check *)
  if Check_Params then                           // Parameter prüfen
  if not Check_HelpParam then                    // Kein Hilfe Para gefunden
  if Parse_Params then                           // Parameter geparst
  if Start_hSplit then                           // Split Thread gestarted
  begin
   while (not FhSplitThread.Running)
   and (not FhSplitThread.Finished)
   and (ExitCode=0) do
   Sleep(1);                                     // ganz kurz auf Thread warten

   (* Loop *)
   while (not FhSplitThread.ProgressDone)        // Progress nicht 100
   and (not FhSplitThread.Finished) do           // Thread läuft noch
   begin   
    (* Fehler Code prüfen - Fehler Text sichern *)
    if ExitCode > 0 then                         // Es gibt einen Fehler
    begin
     FErrorText:=FhSplitThread.ErrorText;        // Fehler Text sichern
     Break;                                      // Abbruch
    end;

    (* Progress Nummer loggen *)
    if FProgress_Interval > 0 then               // Interval muss da sein
    Log_ProgressNums;                            // Progress Nummer loggen

    (* User Input *)
    user_input:=ReadSequence(false);             // Eingabe lesen
    if user_input <> '' then                     // Eingabe nicht leer
    if user_input = #13 then                     // Eingabe war ENTER
    begin
     if command = 'e' then                       // Kommando ist "e" = Exit
     begin
      DoUserExit;                                // Exit ausgeben
      Break;
     end;
     command:='';                                // Kommando leeren
    end
    else                                         // user_input anhängen
    begin
     command:=command+user_input;                // Kommando erweitern
     WriteLn(#13#10+command);                    // Kommando anzeigen
    end;
    user_input:='';                              // User input leeren
   end;

   (* Log 100% *)
   if FProgress_Interval > 0 then                // Interval muss da sein
   Log_ProgressNums;                             // Progress Nummer loggen
  end;
  Log_End;                                       // Ende loggen

  {$ifdef debug}
  ReadLn;                                        // ReadLn - Thread anhalten
  {$endif}
  Terminate(ExitCode);                           // Main Thread beenden
end;


(*------------ public --------------------------------------------------------*)


// Constructor - Progress Interval default init
constructor THSplit.Create(TheOwner: TComponent);
begin
 inherited Create(TheOwner);
 StopOnException:=True;
 FJoinFiles:=nil;

 FProgress_Interval:=100*Interval_Factor;        // Standard Interval 100
 FProgress_IntervalCounter:=FProgress_Interval;
end;


// Destructor
destructor THSplit.Destroy;
begin
 FProgress_Interval:=0;
 FProgress_IntervalCounter:=0;
 FErrorText:='';
 FPara_ChunkSize:='';
 FPara_ChunkCount:='';
 FPara_SegSplitArray:='';
 FPara_ProgressInterval:='';
 FPara_TrgFolder:='';
 FPara_TrgFilename:='';
 FPara_Source:='';
 if FJoinFiles <> nil then
 FreeAndNil(FJoinFiles);

 inherited Destroy;
end;


// Write Help
procedure THSplit.WriteHelp;
var
  help: String;
begin
  help:=hSplitTitle+#13#10
  +'-----------------------------------------------------------------'#13#10
  +'Syntax'#13#10
  +'hSplit.exe [Param1 [Value1 [Param2 Value2 ... ...]]] "Source.file" ["..."]'#13#10
  +'-----------------------------------------------------------------'#13#10
  +'Parameter'#13#10
  +'-h > Show this Help'#13#10#13#10
  +'-S > Segments: split file into segments (default is split Chunks)'#13#10
  +'     an array of comma separated start and end bytes is used'#13#10
  +'     values for start and end can be an Integer or a Hex string'#13#10
  +'     to use hex values set a first char to "h" -> h0-1000,2abc-4def'#13#10
  +'     the end byte is NOT included in the target file'#13#10
  +'     if an end value is set to 0, it means end of file(last byte)'#13#10
  +'     a byte pair of 0-0 means copy the entire file and will be skipped'#13#10
  +'     the start byte must be smaller than the end byte'#13#10#13#10
  +'-s > Chunk size: value is an Integer and default means Megabyte (def=20)'#13#10
  +'     a first or last char can be set to define the value type'#13#10
  +'     b=Byte; k=Kilobyte g=Gigabyte'#13#10#13#10
  +'-c > Chunks count: How many chunks should be written'#13#10
  +'     if not set or set to 0 means infinity until the source file ends (def=0)'#13#10#13#10
  +'-t > Target folder: if not set or empty the source folder is used'#13#10#13#10
  +'-n > Target file name: if not set or empty the source filename is used'#13#10#13#10
  +'-P > Progress interval: Control how often the progress is send to the output'#13#10
  +'     value is a Byte from 0 to 255'#13#10
  +'     value 0 sends no progress numbers; 1=max 255=min (def=100)'#13#10#13#10
  +'-J > Join files: value is a path for a target file'#13#10
  +'     only the -P switch can be used additionally'#13#10#13#10
  +'".." > Source file: MUST be the last parameter! Path to source file'#13#10#13#10
  +'@".." > Path to a parameter file: MUST be the first parameter!'#13#10
  +'        The parameter file should be a simple text file'#13#10
  +'        You can use any name and extension for the file name'#13#10
  +'        Only source file parameters can be used additionally'#13#10
  +'-----------------------------------------------------------------'#13#10
  +'Parameter file'#13#10
  +'Each line is used as a parameter'#13#10
  +'If you don''t set a source path in the parameter file,'#13#10
  +'the source path(s) is(are) read from the command-line.'#13#10
  +'You can use a root parameter file called "params.txt"'#13#10
  +'Exists this file in the root folder of hSplit, it will be used automatically.'#13#10
  +'If the "params.txt" is empty, it will be ignored.'#13#10
  +'-----------------------------------------------------------------'#13#10
  +'Command'#13#10
  +'You can send a command while hSplit is running.'#13#10
  +'Command will be executed after pressing the ENTER key'#13#10#13#10
  +'"e" > User Exit: stops hSplit'#13#10
  +'-----------------------------------------------------------------'#13#10
  +'Exit codes'#13#10
  +'hSplit sends at the end an exit code'#13#10#13#10
  +'0 - all OK'#13#10
  +'1 - no source file'#13#10
  +'2 - error while loading source file'#13#10
  +'3 - no target folder (not found or could not created)'#13#10
  +'4 - chunk size too big (bigger than the source file size)'#13#10
  +'5 - nothing to split (no data)'#13#10
  +'6 - nothing to join (no data)'#13#10
  +'7 - parameter file not found'#13#10
  +'8 - parameter file is empty (only if used with the cli parameter)'#13#10
  +'9 - wrong parameters '#13#10
  +'99 - internal Lazarus error'#13#10
  +'-1 - User Exit'#13#10
  +'-----------------------------------------------------------------'#13#10
  +'Examples'#13#10
  +'Split chunks:'#13#10
  +'hSplit.exe "Path\to\source.file"'#13#10
  +'split entire source file in 20mb chunks'#13#10#13#10
  +'hSplit.exe -s 100 "Path\to\source.file"'#13#10
  +'split entire source file in 100mb chunks'#13#10#13#10
  +'hSplit.exe -s g2 -c 10 "Path\to\source.file"'#13#10
  +'hSplit.exe -s 2g -c 10 "Path\to\source.file"'#13#10
  +'split 10 chunks with 2 gigabyte each'#13#10#13#10
  +'hSplit.exe -t "trg dir" -c 10 -n "trg name" "Path\to\source.file"'#13#10
  +'split 10 chunks with 20mb each to folder "tar dir" with name "trg name"'#13#10
  +'the file extension is used from the source file'#13#10#13#10
  +'Split segments:'#13#10
  +'hSplit.exe -S 0-1000,5000-6000,500-1500 "Path\to\source.file"'#13#10
  +'split three segments with 1000 bytes each'#13#10#13#10
  +'hSplit.exe -S h0-1000,5000-6000,500-1500 "Path\to\source.file"'#13#10
  +'split three segments with 4096 bytes each'#13#10#13#10
  +'Join files:'#13#10
  +'hSplit.exe -J "Path\to trg.file" -P 0 "Path\source.1" "Path\source.2"'#13#10
  +'join 2 files without progress numbers'#13#10#13#10
  +'Parameter file:'#13#10
  +'hSplit.exe @"Path\to MyParam.file"'#13#10
  +'all parameters are read from the parameter file'#13#10#13#10
  +'hSplit.exe @"Path\to MyParam.file" "Path\to\source.file"'#13#10
  +'the source parameter from command-line is used (no path in the parameter file)'#13#10
  +'(if you want to join files, you can specify more command-line parameters)';

  writeln(help);
  {$ifdef windows}
  ReadLn;
  {$endif}
end;


var
 Application: THSplit;




begin
 Application:=THSplit.Create(nil);
 Application.Title:=hSplitTitle;
 Application.Run;
 Application.Free;
end.





