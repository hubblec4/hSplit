unit hSplitThread;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, LazFileUtils;

const                   // Fehler Codes
 hsErr_NoSourceFile     = 1;                     // Source Datei nicht vorhanden
 hsErr_LoadSource       = 2;                     // Source Datei Fehler Laden
 hsErr_NoTrgFolder      = 3;                     // Ziel Ordner nicht vorhanden
 hsErr_ChunkSizeTooBig  = 4;                     // Teil-Größe zu groß
 hsErr_NothingToSplit   = 5;                     // keine Daten zum splitten
 hsErr_NothingToJoin    = 6;                     // keine Daten zum joinen
 hsErr_NoParams_txt     = 7;                     // Params.txt nicht gefunden
 hsErr_Params_txtEmpty  = 8;                     // Params.txt leer
 hsErr_WrongParams      = 9;                     // Falsche Parameter
 hsErr_LazException     = 99;                    // Lazarus Prozess Exception


const                   // Auslese Größe
  read_size = 2097152;

type

 QWordArray             = array of QWord;



 ThSplitThread          = class(TThread)
  strict private
   (* Getter *)
   function Get_ProgressNumChunk: Single;
   function Get_ProgressNumTotal: Single;
   function Get_ProgressDone: Boolean;

  private               // private vars
   FUserExit            : Boolean;
   FErrorText           : String;

   FSource              : String;
   FSrcExtension        : String;
   FSrcStream           : TFileStream;
   FSrcSize             : QWord;
   FTargetFolder        : String;
   // für Join ist FTrgFilename der gesamte Datei Pfad
   FTrgFilename         : String;                // Dateiname OHNE Erweiterung
   FtrgStream           : TFileStream;

   FChunkSize           : QWord;                 // Size für die Bytes
   FChunkCount          : Integer;               // 0 unendlich, bis Ende Datei
   FChunkBytes_readed   : QWord;

   FSegmentSplit        : Boolean;               // false=Chunk, true=Segment
   FSegmentStarts       : QWordArray;
   FSegmentEnds         : QWordArray;

   FBytes_Total         : QWord;
   FBytes_TotalCounter  : QWord;

   FProgressNumChunk    : Single;
   FProgressNumTotal    : Single;

   FJoinFiles           : TStringList;           // Datei Pfade

   FBuffer              : array [0..read_size] of Byte; // Größe = read_size
   FRunning             : Boolean;

  private               // Proceduren
   function Load_SourceFile: Boolean;
   function Check_Target: Boolean;

   function SplitChunks: Boolean;
   function SplitSegments: Boolean;
   procedure ReadAndWriteBytes;

   function JoinFiles: Boolean;


  protected             // Proceduren
   procedure Execute; override;

  public                // Proceduren
   constructor Create;
   destructor Destroy; override;

   procedure Insert_SegmentSplits(const Starts,Ends: QWordArray);
   procedure Insert_JoinFiles(const Join_Files: TStringList);

  public                // Properties
   property Source      : String read FSource write FSource;
   property TargetFolder: String read FTargetFolder write FTargetFolder;
   property TargetFilename: String read FTrgFilename write FTrgFilename;

   property SegmentSplit: Boolean read FSegmentSplit write FSegmentSplit;
   property ChunkSize   : QWord read FChunkSize write FChunkSize;
   property ChunkCount  : Integer read FChunkCount write FChunkCount;

   property Running     : Boolean read FRunning;
   property ProgressNumChunk: Single read Get_ProgressNumChunk;
   property ProgressNumTotal: Single read Get_ProgressNumTotal;
   property ProgressDone: Boolean read Get_ProgressDone;

   property UserExit    : Boolean write FUserExit;
   property ErrorText   : String read FErrorText;
 end;




implementation



(*------------ strict private ------------------------------------------------*)


// Progress Nummer für den aktuellen Chunk
function ThSplitThread.Get_ProgressNumChunk: Single;
begin
  if (FChunkSize = 0)
  or (FChunkBytes_readed = 0) then
  FProgressNumChunk:=0
  else
  if FChunkBytes_readed = FChunkSize then        // alle Bytes verarbeitet
  FProgressNumChunk:=100                         // 100%
  else                                           // Prozente errechnen
  FProgressNumChunk:=(FChunkBytes_readed         // ausrechnen
  /FChunkSize)*100;
  Result:=FProgressNumChunk;                     // Result ausgeben
end;


// Progress Nummer Total
function ThSplitThread.Get_ProgressNumTotal: Single;
begin
  if (FBytes_Total = 0)
  or (FBytes_TotalCounter = 0) then
  FProgressNumTotal:=0
  else
  if FBytes_TotalCounter = FBytes_Total then     // alle Bytes verarbeitet
  FProgressNumTotal:=100                         // 100%
  else                                           // Prozente errechnen
  FProgressNumTotal:=(FBytes_TotalCounter        // ausrechnen
  /FBytes_Total)*100;
  Result:=FProgressNumTotal;                     // Result ausgeben
end;


// Progress fertig
function ThSplitThread.Get_ProgressDone: Boolean;
begin
  Result:=false;
  if FBytes_Total = 0 then Exit;
  Result:=FBytes_TotalCounter >= FBytes_Total;
end;



(*------------ private -------------------------------------------------------*)


// Source Datei laden
function ThSplitThread.Load_SourceFile: Boolean;
begin
  Result:=false;                                 // Result init
  if not FileExists(FSource) then                // Quell Datei ist nicht da
  begin
   ExitCode:=hsErr_NoSourceFile;                 // Fehler Code zuweisen
   Exit;                                         // Abbruch Datei nicht da
  end;

  try
   FSrcExtension:=ExtractFileExt(FSource);       // Datei Erweiterung ermitteln
   FSrcStream:=TFileStream.Create(FSource, fmOpenRead or fmShareDenyWrite);
   FSrcSize:=FSrcStream.Size;                    // Datei Größe ermitteln
   Result:=true;                                 // Result ausgeben OK
  Except
   on e: Exception do                            // Fehler abfangen
   begin
    FErrorText:=e.Message;                       // Fehler Text sichern
    ExitCode:=hsErr_LoadSource;                  // Fehler Code zuweisen
   end;
  end;
end;


// Target Ordner prüfen
function ThSplitThread.Check_Target: Boolean;
begin
  Result:=false;                                 // Result init
  (* Ziel Ordner *)
  if FTargetFolder = '' then                     // kein Ziel Ordner angegeben
  FTargetFolder:=ExtractFileDir(FSource)         // Quell = Ziel Ordner
  else
  if not DirectoryExists(FTargetFolder) then     // Ziel Ordner nicht vorhanden
  if not CreateDir(FTargetFolder) then           // Ordner nicht erstellbar
  begin
   ExitCode:=hsErr_NoTrgFolder;                  // Fehler Code zuweisen
   Exit;                                         // Abbruch
  end;
  (* Ziel Datei *)
  if FtrgFilename = '' then                      // Kein Zielname
  FtrgFilename:=ExtractFileNameOnly(FSource);    // Zielnamen aus Quelle ermitte

  Result:=true;                                  // Result ausgeben OK
end;


// Split Chunks - Teile nach einer gewissen Größe und Anzahl
function ThSplitThread.SplitChunks: Boolean;
var
  chunk_counter: Integer=0;
  chunk_filepath,chunk_filepathFull: String;
  lastChunkSize: QWord;
begin
  Result:=false;                                 // Result init false
  (* Teil-Größe prüfen *)
  if FChunkSize >= FSrcSize then                 // Teil-Größe zu groß
  begin
   ExitCode:=hsErr_ChunkSizeTooBig;              // Fehler Code setzen
   Exit;                                         // Abbruch
  end;
  FSrcStream.Position:=0;                        // 1.Byte anwählen

  (* FBytes_Total - die tatsächlichen Bytes die verarbeitet werden sollen *)
  if FChunkCount = 0 then                        // keine Chunk Begrenzung
  FBytes_Total:=FSrcSize                         // geamte Quell Datei Bytes
  else
  begin
   FBytes_Total:=FChunkSize * FChunkCount;       // Anzahl der Chunk * Größe
   if FBytes_Total > FSrcSize then               // zu viele Bytes
   FBytes_Total:=FSrcSize;                       // Quell Datei größe nutzen
  end;

  (* Ziel Dateiname vorbereiten - 1.Teil *)
  chunk_filepath:=FTargetFolder                  // Datei name bilden
  +DirectorySeparator+FtrgFilename+'_';

  try
   FChunkBytes_readed:=FChunkSize;               // FChunkBytes_readed init
   while FBytes_TotalCounter < FBytes_Total do   // alle Bytes verarbeiten
   begin
    (* neuen Chunk *)
    if FChunkBytes_readed = FChunkSize then      // Chunk Größe erreicht
    begin
     Inc(chunk_counter);                         // Datei Zähler erhöhen
     chunk_filepathFull:=chunk_filepath          // Datei name fertig bilden
     +inttostr(chunk_counter)+FsrcExtension;

     FtrgStream:=TFileStream.Create              // Datei erzeugen
     (chunk_filepathFull,fmCreate);
     FtrgStream.Position:=0;                     // 1.Byte anwählen
     FChunkBytes_readed:=0;                      // FChunkBytes_readed zurückset

     (* Last Chunk Size *)
     lastChunkSize:=FBytes_Total-FBytes_TotalCounter;// restlichen Byte Anzahl
     if lastChunkSize < FChunkSize then          // kleiner als die Chunck Size
     FChunkSize:=lastChunkSize;                  // Chunksize ändern
     // erst jetzt running wegen den progress nummern
     FRunning:=true;                             // Running auf true setzen
    end;

    (* Bytes lesen und schreiben *)
    ReadAndWriteBytes;

    (* Chunk fertig - alle Bytes geschrieben *)
    if FChunkBytes_readed = FChunkSize then      // Chunk Größe erreicht
    FreeAndNil(FtrgStream);                      // Datei Stream freigeben

    (* Chunk Anzahl erreicht - Fertig *)
    if FChunkCount > 0 then
    if chunk_counter > FChunkCount then Break;   // Fertig

    (* Benutzer Exit *)
    if FUserExit then                            // Abbruch durch benutzer
    begin
     if FtrgStream <> nil then                   // Datei Stream nicht nil
     FreeAndNil(FtrgStream);                     // Datei Stream freigeben
     Exit;                                       // Feritg
    end;
   end;

   Result:=true;
  Except
   on e: Exception do                            // Fehler abfangen
   begin
    FErrorText:=e.Message;                       // Fehler Text sichern
    ExitCode:=hsErr_LazException;                // Fehler Code zuweisen
   end;
  end;
end;


// Split Segment(s)
function ThSplitThread.SplitSegments: Boolean;
var
  s,maxSegs,starts_count,ends_count: Integer;
  chunk_filepath,chunk_filepathFull: String;
  ByteStart,ByteEnd: QWord;

  (* Bytes Total - Ersetzen der Ends=0 durch die Datei-Size
     Alle ungültigen Zeit-Paare -> End-Byte auf 0 setzen *)
  procedure Get_Bytes_Total;
  var
    a : Integer;
  begin
    FBytes_Total:=0;                             // FBytes_Total init = 0
    for a:=0 to maxSegs -1 do                    // Segment Arrays durchgehen
    begin
     ByteStart:=FSegmentStarts[a];               // Start Byyte ermitteln
     ByteEnd:=FSegmentEnds[a];                   // End Byyte ermitteln
     (* gesamte Datei kopieren ausschliessen *)
     if (ByteStart + ByteEnd) = 0 then           // Alle bytes von der Datei
     Continue;                                   // weiter
     (* End Byte prüfen und neu setzen *)
     if (ByteEnd = 0)                            // 0 = bis Ende Datei
     or (ByteEnd > FSrcSize) then                // End ist zu groß
     begin
      FSegmentEnds[a]:=FSrcSize;                // Datei größe setzen
      ByteEnd:=FSrcSize;                         // neues Ende setzen
     end;

     (* Start ist zu groß - größer als Datei wird damit auch anbgefangen *)
     if ByteStart >= ByteEnd then                // Start Byte zu groß/gleich
     begin
      FSegmentEnds[a]:=0;                        // EndByte löschen
      Continue;
      // weiter
     end;
     Inc(FBytes_Total,(ByteEnd - ByteStart));    // Total Byte mitzählen
    end;
  end;

begin
  Result:=false;                                 // Result init false
  (* Maximale Anzahl an Segmenten *)
  starts_count:=Length(FSegmentStarts);          // Anzahl Starts
  ends_count:=Length(FSegmentEnds);              // Anzahl Ends
  if starts_count = ends_count then              // beide gleich lang
  maxSegs:=starts_count                          // max Segemente setzen
  else
  if starts_count > ends_count then              // es gibt mehr starts als end
  maxSegs:=ends_count                            // Ende Anzahl nutzen
  else                                           // es gibt weniger starts
  maxSegs:=starts_count;                         // Starts Anzahl nutzen

  (* Bytes Total *)
  Get_Bytes_Total;                               // Total Bytes ermitteln
  if FBytes_Total = 0 then                       // keine Segmente zum splitten
  begin
   ExitCode:=hsErr_NothingToSplit;               // Fehler Code setzen
   Exit;                                         // fertig - Abbruch
  end;

  (* Ziel Dateiname vorbereiten - 1.Teil *)
  chunk_filepath:=FTargetFolder                  // Datei name bilden
  +DirectorySeparator+FtrgFilename+'_';

  FRunning:=true;                                // Running auf true setzen
  try
   for s:=0 to maxSegs -1 do                     // Segmente durchgehen
   begin
    ByteEnd:=FSegmentEnds[s];                    // End Byyte ermitteln
    // EndByte 0 ist nicht Ende Datei, das wurde vorher geprüft
    if ByteEnd = 0 then Continue;                // End=0 überspringen
    FChunkBytes_readed:=0;                       // Chunk Bytes gelesen reset
    ByteStart:=FSegmentStarts[s];                // Start Byyte ermitteln
    FChunkSize:=ByteEnd - ByteStart;             // Segemnt Größe
    (* Streams vorbereiten *)
    chunk_filepathFull:=chunk_filepath           // Datei name fertig bilden
     +inttostr(s+1)+FsrcExtension;
    FtrgStream:=TFileStream.Create               // Datei erzeugen
     (chunk_filepathFull,fmCreate);
     FtrgStream.Position:=0;                     // 1.Byte anwählen
    FSrcStream.Position:=ByteStart;              // Start Byte setzen

    while FChunkBytes_readed < FChunkSize do     // Chunk Bytes verarbeiten
    begin
     (* Bytes lesen und schreiben *)
     ReadAndWriteBytes;

     (* Benutzer Exit *)
     if FUserExit then                           // Abbruch durch benutzer
     begin
      if FtrgStream <> nil then                  // Datei Stream nicht nil
      FreeAndNil(FtrgStream);                    // Datei Stream freigeben
      Exit;                                      // Feritg
     end;
    end;
    FreeAndNil(FtrgStream);                      // Datei Stream freigeben
   end;

   Result:=true;
  Except
   on e: Exception do                            // Fehler abfangen
   begin
    FErrorText:=e.Message;                       // Fehler Text sichern
    ExitCode:=hsErr_LazException;                // Fehler Code zuweisen
   end;
  end;
end;

// Bytes lesen und schreiben
procedure ThSplitThread.ReadAndWriteBytes;
var
  Bytes_Readed,read_Bytes: QWord;
begin
  (* zu lesende Bytes ermitteln *)
  read_Bytes:=FChunkSize - FChunkBytes_readed; // read_Bytes init
  if read_Bytes > read_size then               // read_Bytes zu groß
  read_Bytes:=read_size;                       // read_Bytes max read_size

  (* Bytes auslesen und mitzählen *)
  Bytes_Readed:=FSrcStream.Read(FBuffer        // Bytes lesen
  ,read_Bytes);
  Inc(FChunkBytes_readed,Bytes_Readed);        // gelesen B. mitzählen Chunk
  Inc(FBytes_TotalCounter,Bytes_Readed);       // gelesen B. mitzählen Total

  (* Bytes schreiben *)
  FtrgStream.Write(FBuffer, Bytes_Readed);     // Bytes in Datei schreiben
end;


(* Join *)

// Join Files
function ThSplitThread.JoinFiles: Boolean;
var
  f,f_Count: Integer;
  fPath,fPathOld: String;

  // Totel Bytes ermitteln - Datei Pfad prüfen, Anzahl Join Dateien
  procedure Get_Bytes_Total;
  var
    p: Integer;
    fSize: Int64;
  begin
    fPathOld:='';                                // alten Path leeren
    (* Pfade prüfen *)
    for p:=FJoinFiles.Count -1 downto 0 do       // Pfad durchgehen
    begin
     fPath:=FJoinFiles[p];                       // Pfad ermitteln
     if fPath <> fPathOld then                   // neuer Datei Pfad
     begin
      if not FileExists(fPath) then              // Datei nicht vorhanden
      begin
       FJoinFiles.Delete(p);                     // Pfad löschen
       Continue;                                 // weiter
      end;
      fSize:=FileSizeUtf8(fPath);                // Datei Größe ermitteln
     end;
     fPathOld:=fPath;                            // neuen "alten" Pfad sichern
     FBytes_Total:=FBytes_Total+fSize;           // Datei größe mitzählen
    end;
    f_Count:=FJoinFiles.Count;                   // Anzahl Join Dateien sichern
  end;

begin
  Result:=false;                                 // Result init false
  (* Bytes Total *)
  Get_Bytes_Total;                               // Total Bytes ermitteln
  if (FBytes_Total = 0)                          // keine Daten zum joinen
  or (f_Count < 2) then                          // keine Dateien zum joinen
  begin
   ExitCode:=hsErr_NothingToJoin;                // Fehler Code setzen
   Exit;                                         // fertig - Abbruch
  end;
  FtrgStream:=TFileStream.Create                 // Ziel Datei erzeugen
  (FTrgFilename,fmCreate);

  FRunning:=true;                                // Running auf true setzen
  try
   fPathOld:='';                                 // alten Path leeren
   for f:=0 to f_Count -1 do                     // Dateien durchgehen
   begin
    fPath:=FJoinFiles[f];                        // Pfad ermitteln
    if fPath <> fPathOld then                    // neuer Datei Pfad
    begin
     FSrcStream:=TFileStream.Create(fPath        // Join Datei öffnen
     ,fmOpenRead or fmShareDenyWrite);
     FChunkSize:=FSrcStream.Size;                // Datei Größe zuweisen
    end;
    FSrcStream.Position:=0;                      // 1.Byte anwählen
    FChunkBytes_readed:=0;                       // Chunk Bytes gelesen reset

    while FChunkBytes_readed < FChunkSize do     // Chunk Bytes verarbeiten
    begin
     (* Bytes lesen und schreiben *)
     ReadAndWriteBytes;

     (* Benutzer Exit *)
     if FUserExit then                           // Abbruch durch benutzer
     begin
      FreeAndNil(FSrcStream);                    // Join Datei Stream freigeben
      FreeAndNil(FtrgStream);                    // Ziel Datei Stream freigeben
      Exit;                                      // Feritg
     end;
    end;
    FreeAndNil(FSrcStream);                      // Datei Stream freigeben
   end;

   Result:=true;
  Except
   on e: Exception do                            // Fehler abfangen
   begin
    FErrorText:=e.Message;                       // Fehler Text sichern
    ExitCode:=hsErr_LazException;                // Fehler Code zuweisen
   end;
  end;
  FreeAndNil(FtrgStream);                        // Ziel Datei Stream freigeben
end;



(*------------ protected -----------------------------------------------------*)

// Execute - Ausführen
procedure ThSplitThread.Execute;
begin
  if not FUserExit then                          // Abbruchbedingung
  (* Ziel Ordner prüfen *)
  if Check_Target then                           // Ziel prüfen

  (* Join Files *)
  if FJoinFiles <> nil then                      // Datei Liste vorhanden
  JoinFiles                                      // Dateien joinen
  else

  (* Source Datei laden *)
  if Load_SourceFile then                        // Datei laden
  if not FUserExit then                          // User Exit

  if FSegmentSplit then                          // Segment split aktiv
  SplitSegments                                  // Segment Splitting
  else
  SplitChunks;                                   // Chunk splitten

  Terminate;                                     // Thread beenden
end;



(*------------ public --------------------------------------------------------*)

// Constructor
constructor ThSplitThread.Create;
begin
  FErrorText:='';
  FUserExit:=false;
  FSrcStream:=nil;
  FtrgStream:=nil;
  FProgressNumTotal:=0;
  FProgressNumChunk:=0;
  FChunkBytes_readed:=0;
  FChunkSize:=20*1024*1024;                      // Default 20 mb init
  FBytes_TotalCounter:=0;                        // FBytes_TotalCounter init = 0
  FJoinFiles:=nil;
  FreeOnTerminate := True;
  inherited Create(true);
end;

// Destructor
destructor ThSplitThread.Destroy;
begin
  FUserExit:=false;
  FErrorText:='';
  FreeAndNil(FSrcStream);
  FSrcSize:=0;
  FSource:='';
  FsrcExtension:='';
  FTargetFolder:='';
  FtrgFilename:='';
  FProgressNumTotal:=0;
  FProgressNumChunk:=0;
  FChunkSize:=0;
  FChunkCount:=0;
  FChunkBytes_readed:=0;
  FBytes_Total:=0;
  FBytes_TotalCounter:=0;
  if FJoinFiles <> nil then
  FreeAndNil(FJoinFiles);
  SetLength(FSegmentStarts,0);
  SetLength(FSegmentEnds,0);
  inherited Destroy;
end;


// Segment Zeiten einfügen
procedure ThSplitThread.Insert_SegmentSplits(const Starts,Ends: QWordArray);
begin
  FSegmentStarts:=Starts;
  FSegmentEnds:=Ends;
end;


// Join Dateien einfügen
procedure ThSplitThread.Insert_JoinFiles(const Join_Files: TStringList);
begin
  if FJoinFiles = nil then                       // Join List nicht da
  FJoinFiles:=TStringList.Create;                // Liste erzeugen
  FJoinFiles.Text:=Join_Files.Text;              // Daten übertragen
end;

























end.

