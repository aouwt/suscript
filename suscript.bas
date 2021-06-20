$VERSIONINFO:CompanyName=Amogus Esolang Crew
$VERSIONINFO:FileDescription=Suscript compiler
$VERSIONINFO:InternalName=Amogus Esolang
$VERSIONINFO:OriginalFilename=sus.exe
$VERSIONINFO:ProductName=Suscript
$VERSIONINFO:Web=https://github.com/all-other-usernames-were-taken/suscript

'$DYNAMIC
$CONSOLE:ONLY
_DEST _CONSOLE

CONST TRUE = -1, Sus = TRUE
CONST FALSE = 0, Safe = FALSE
DIM SHARED NL AS STRING * 1: NL = CHR$(10)

'Colors for logging
'See https://www.qb64.org/wiki/PALETTECOLOR
CONST __Color_Black = 0
CONST __Color_DarkBlue = 1
CONST __Color_DarkGreen = 2
CONST __Color_DarkCyan = 3
CONST __Color_DarkRed = 4
CONST __Color_DarkMagenta = 5
CONST __Color_DarkYellow = 6
CONST __Color_LightGrey = 7
CONST __Color_DarkGrey = 8
CONST __Color_Blue = 9
CONST __Color_Green = 10
CONST __Color_Cyan = 11
CONST __Color_Red = 12
CONST __Color_Magenta = 13
CONST __Color_Yellow = 14
CONST __Color_White = 15

'Define colors for logging
CONST __Log_Color_Warn = __Color_Magenta
CONST __Log_Color_Error = __Color_Yellow
CONST __Log_Color_Fatal = __Color_Red
CONST __Log_Color_Info = __Color_White
CONST __Log_Color_Normal = __Color_LightGrey
CONST __Log_Color_Debug = __Color_Blue

DIM SHARED __Log_Off AS _BIT: __Log_Off = FALSE
DIM SHARED __Log_ForceSTDOUT AS _BIT: __Log_ForceSTDOUT = FALSE
DIM SHARED __Log_Suppress_Info AS _BIT: __Log_Suppress_Info = FALSE
DIM SHARED __Log_Suppress_Debug AS _BIT: __Log_Suppress_Debug = TRUE
DIM SHARED __Log_Suppress_Warn AS _BIT: __Log_Suppress_Warn = FALSE
DIM SHARED __Log_Suppress_Error AS _BIT: __Log_Suppress_Error = FALSE
DIM SHARED __Log_Suppress_Fatal AS _BIT: __log_suppress_fata = FALSE

DIM SHARED __TempDir AS STRING
$IF LINUX THEN
    __TempDir = "/tmp/"
$ELSEIF WIN THEN
    __tempdir="./"
$END IF
DIM SHARED __OutputFile AS STRING: __OutputFile = ""
DIM SHARED __TempFilePath AS STRING, __TempFile AS _UNSIGNED INTEGER: __TempFilePath = __TempDir + "sus.bas"
DIM SHARED __InputFilePath AS STRING, __InputFile AS _UNSIGNED INTEGER: __InputFilePath = ""
DIM SHARED __LogFilePath AS STRING, __LogFile AS _UNSIGNED INTEGER: __LogFilePath = ""

DIM SHARED __EmbedLibsus AS _BIT: __EmbedLibsus = TRUE
DIM SHARED __Libsus AS STRING: __Libsus = _CWD$ + "/libsus.bh"

DIM SHARED __QB64 AS STRING: __QB64 = "./qb64"
DIM SHARED __QB64_Args AS STRING: __QB64_Args = "-x -w"

DIM SHARED __ErrorOnExistingOutputFile AS _BIT: __ErrorOnExistingOutputFile = FALSE
DIM SHARED __DontCompile AS _BIT: __DontCompile = FALSE


CONST __Libsus_Input = "I"
CONST __Libsus_IncPtr = "IP", __Libsus_DecPtr = "DP"
CONST __Libsus_Amogus = "A", __Libsus_Town = "P"
CONST __Libsus_CurrentTown = "A(P)"


FOR i~% = 1 TO _COMMANDCOUNT
    'Log "i debug", "Parsing CLI option '" + COMMAND$(i~%) + "'"
    IF (ASC(COMMAND$(i~%), 1) = 45) AND (NOT COMMAND$(i~%) = "-") THEN 'if arg begins with - but is not just -
        SELECT CASE COMMAND$(i~%)
            CASE "-h", "--help"
                PRINT "Suscript vQB-2 (c)2021 Amogus Esolang Crew"
                PRINT "https://github.com/all-other-usernames-were-taken/suscript"
                PRINT ""
                PRINT "Usage: " + COMMAND$(0) + " [OPTIONS] [-i|--input] INPUT [OUTPUT]"
                PRINT ""
                PRINT "Options:"
                PRINT "  -h, --help         Displays this help screen."
                PRINT ""
                PRINT "  -o, --output FILE  Outputs the executable as FILE."
                PRINT ""
                PRINT "  -i, --input FILE   Takes the suscript program from FILE."
                PRINT "                       Use '-' to read from stdin."
                PRINT ""
                PRINT "  -t, --temp DIR     Use DIR as a temporary directory."
                $IF LINUX THEN
                    PRINT "                       Default is '/tmp/'."
                $ELSEIF WIN THEN
                    print "                       Default is the current directory."
                $END IF
                PRINT ""
                PRINT "  -c, --dont-compile  Does not compile the outputted QB64 code. The output file"
                PRINT "                        will be used as the QB64 code output."
                PRINT ""
                PRINT "  -v, --verbose LEVEL  Sets the verbosity level."
                PRINT "                         LEVEL can be normal, debug, errors, fatal, or none."
                PRINT ""
                PRINT "  -l, --log FILE     Outputs log messages to FILE. Use '-' to output to stdout."
                PRINT ""
                PRINT "  -L, --libsus FILE  Uses FILE as libsus instead of libsus.bi."
                PRINT ""
                PRINT "  -Q, --qb64 FILE    Use FILE as the QB64 executable to use for compilation."
                PRINT "                       Default is './qb64'."
                PRINT ""
                PRINT "  --qb64-arg ARGUMENTS  Pass ARGUMENTS to the QB64 executable at compile time."
                PRINT "                          Default is '-x -w'"
                PRINT ""
                PRINT "  --no-overwrite     Fails if the output file already exists."
                PRINT ""
                SYSTEM

            CASE "-c", "--dont-compile"
                __DontCompile = TRUE

            CASE "-l", "--log"
                i~% = i~% + 1
                __LogFilePath = COMMAND$(i~%)
                IF __LogFilePath = "-" THEN __LogFilePath = ""

            CASE "-o", "--output"
                i~% = i~% + 1
                __OutputFile = COMMAND$(i~%)

            CASE "-t", "--temp"
                i~% = i~% + 1
                __TempDir = COMMAND$(i~%)

            CASE "-i", "--input"
                i~% = i~% + 1
                __InputFilePath = COMMAND$(i~%)
                IF __InputFilePath = "-" THEN __InputFilePath = "/dev/stdin"

            CASE "-v", "--verbose"
                i~% = i~% + 1
                SELECT CASE COMMAND$(i~%)
                    CASE "normal"
                        __Log_Suppress_Info = FALSE

                    CASE "debug"
                        __Log_Suppress_Debug = FALSE

                    CASE "errors"
                        __Log_Suppress_Warn = TRUE

                    CASE "fatal"
                        __Log_Suppress_Warn = TRUE
                        __Log_Suppress_Error = TRUE

                    CASE "none"
                        __Log_Off = TRUE
                END SELECT

            CASE "-N", "--report"
                __Log_Off = FALSE
                __Log_ForceSTDOUT = TRUE

            CASE "--ignore-existing"
                __ErrorOnExistingOutputFile = TRUE

            CASE "-L", "--libsus"
                i~% = i~% + 1
                __Libsus = "'$INCLUDE:'" + COMMAND$(i~%) + "'"

            CASE "-Q", "--qb64"
                i~% = i~% + 1
                __QB64 = COMMAND$(i~%)

            CASE "--qb64-arg"
                i~% = i~% + 1
                __QB64_Args = COMMAND$(i~%)

            CASE ELSE
                Log "i fatal", "Unknown argument '" + COMMAND$(i~%) + "'"
                SYSTEM

        END SELECT

    ELSE
        IF __InputFilePath = "" THEN 'is input file defined?
            __InputFilePath = COMMAND$(i~%)
            IF __InputFilePath = "-" THEN __InputFilePath = "/dev/stdin"

        ELSEIF __OutputFile = "" THEN 'is output file defined?
            __OutputFile = COMMAND$(i~%)
        END IF

    END IF
NEXT

IF NOT _DIREXISTS(__TempDir) THEN
    Log "i fatal", "Temp dir doesn't exist!"
    SYSTEM
END IF
__TempFilePath = __TempDir + ".suscript_temp.bas"

IF __InputFilePath = "" THEN
    Log "i fatal", "No input file!"
    SYSTEM
END IF

IF NOT _FILEEXISTS(__InputFilePath) THEN
    Log "i fatal", "Input file doesn't exist!"
    SYSTEM
END IF

IF NOT _FILEEXISTS(__QB64) THEN
    Log "i fatal", "QB64 not found in " + __QB64
    SYSTEM
END IF


IF __OutputFile = "" THEN 'if we dont define output then use basename/basename.exe
    FOR i~% = LEN(__InputFilePath) TO 1 STEP -1
        IF ASC(__InputFilePath, i~%) = 46 THEN 'get last '.'

            IF __DontCompile THEN
                __OutputFile = LEFT$(__InputFilePath, i~%) + "bas" 'make .bas

            ELSE
                $IF LINUX THEN
                    __OutputFile = LEFT$(__InputFilePath, i~% - 1) 'no extension
                $ELSEIF WIN THEN
                    __outputfile=left$(__inputfilepath,i~%)+"exe"     'make .exe
                $END IF
            END IF

            EXIT FOR
        END IF
    NEXT
END IF

IF __LogFilePath <> "" THEN
    __LogFile = FREEFILE: OPEN __LogFilePath FOR APPEND AS #__LogFile
    PRINT #__LogFile, "## suscript vQB-1 ##"
END IF

IF _FILEEXISTS(__OutputFile) THEN
    IF __ErrorOnExistingOutputFile THEN
        Log "i fatal", "Output file exists"
        SYSTEM
    ELSE
        Log "i warn", "Output file exsts, overwriting"
    END IF
END IF


IF __DontCompile THEN __TempFilePath = __OutputFile




'### BEGIN ACTUAL COMPILER ###
DIM SHARED Labels(0) AS STRING
DIM SHARED IfStatements(0) AS _UNSIGNED _INTEGER64 'So we can catch ENDIF without IF or IF without ENDIF errors
DIM SHARED IfStatementPtr AS INTEGER
DIM SHARED CurrentLineNo AS _UNSIGNED _INTEGER64
DIM SHARED CurrentLine AS STRING


Log "i debug", "Loading file"
__InputFile = FREEFILE: OPEN __InputFilePath FOR INPUT AS #__InputFile

Log "info", "Beginning Suscript compilation"

'KILL __TempFilePath
o$ = __Libsus
DO
    CurrentLineNo = CurrentLineNo + 1

    LINE INPUT #__InputFile, ln$

    ln$ = LTRIM$(RTRIM$(ln$))
    IF ln$ = "" THEN _CONTINUE
    CurrentLine = ln$

    REDIM args$(100)
    CALL SplitArgs(ln$, args$())

    o$ = o$ + TranslateCommand(args$()) + NL

LOOP UNTIL EOF(__InputFile)

IF IfStatementPtr <> 0 THEN
    CurrentLine = "": CurrentLineNo = IfStatements(IfStatementPtr)
    Log "fatal", "'when the impostor is sus 😳' without 'among drip'!"
    SYSTEM
END IF

__TempFile = FREEFILE: OPEN __TempFilePath FOR OUTPUT AS #__TempFile
PRINT #__TempFile, o$;

CLOSE __InputFile, __TempFile

IF __DontCompile THEN
    Log "info", "Translation completed!"
    SYSTEM
END IF

'Pass code to qb64
Log "info", "Beginning QB64 compilation"
SHELL __QB64 + " " + __QB64_Args + " -o '" + __OutputFile + "' '" + __TempFilePath + "'"
IF _FILEEXISTS(__OutputFile) THEN
    KILL __TempFilePath
    Log "info", "Compilation completed!"
ELSE Log "fatal", "QB64 code failed to compile! Check above for errors. QB64 code saved as '" + __TempFilePath + "'"
END IF
SYSTEM


SUB SplitArgs (ln$, Args$()) 'this one works better and is easier to maintain, although slow. also allows for escaping
    'stolen from stupidc's GetPgmArgs

    DIM i AS _UNSIGNED _BYTE
    DIM NextArg AS _UNSIGNED _BYTE
    DO UNTIL i >= LEN(ln$)
        i = i + 1
        SELECT CASE ASC(ln$, i)

            CASE 34 'quote
                a~%% = INSTR(i + 1, ln$, CHR$(34))
                IF a~%% = 0 THEN
                    Log "warn", "No closing quotation! Pretending there's one at the end."
                    Args$(NextArg) = MID$(ln$, i + 1)
                    i = LEN(ln$)
                ELSE
                    Args$(NextArg) = MID$(ln$, i + 1, a~%% - i - 1)
                    i = a~%%
                END IF

            CASE 32 'space
                IF Args$(NextArg) <> "" THEN NextArg = NextArg + 1

            CASE 92 'backslash
                Args$(NextArg) = Args$(NextArg) + CHR$(ASC(ln$, i + 1))
                i = i + 1

            CASE ELSE
                Args$(NextArg) = Args$(NextArg) + CHR$(ASC(ln$, i))

        END SELECT
        IF NextArg > UBOUND(args$) THEN REDIM _PRESERVE Args$(NextArg)
    LOOP
END SUB


FUNCTION TranslateCommand$ (args$())
    SELECT CASE args$(0)
        CASE "eject"
            '   eject <string>
            REM PRINT <string>

            o$ = "?" + CHR$(34) + args$(1) + CHR$(34)


        CASE "when"
            '   when the impostor is sus 😳:
            REM IF amogus(ptr) THEN

            IF args$(1) = "the" AND _
               args$(2) = "imposter" AND _
               args$(3) = "is" AND _
               args$(4) = "sus" AND _
               args$(5) = "😳:" THEN

                o$ = "IF " + __Libsus_CurrentTown + "THEN"
                IfStatementPtr = IfStatementPtr + 1
                IF IfStatementPtr > UBOUND(ifstatements) THEN REDIM _PRESERVE IfStatements(IfStatementPtr) AS _UNSIGNED _INTEGER64
            ELSE GOTO SyntaxError
            END IF


        CASE "among"
            '   among drip
            REM END IF

            IF args$(1) = "drip" THEN o$ = "ENDIF" ELSE GOTO SyntaxError
            IfStatementPtr = IfStatementPtr - 1
            IF IfStatementPtr < 0 THEN
                CurrentLine = "": CurrentLineNo = IfStatements(0)
                Log "fatal", "'among drip' without 'when the imposter is sus 😳:'!"
                SYSTEM
            END IF


        CASE "impostor"
            '   impostor sus
            REM amogus(ptr) = -1

            '   impostor safe
            REM amogus(ptr) = 0

            SELECT CASE args$(1)
                CASE "sus": o$ = __Libsus_CurrentTown + "=1"
                CASE "safe": o$ = __Libsus_CurrentTown + "=0"
                CASE ELSE: GOTO SyntaxError
            END SELECT


        CASE "amogus"
            '   amogus goes to the next sus town
            REM GOSUB IncPtr

            '   amogus goes to the previous sus town
            REM GOSUB DecPtr

            IF args$(1) = "goes" AND _
               args$(2) = "to" AND _
               args$(4) = "sus" AND _
               args$(5) = "town" THEN
                SELECT CASE args$(3)
                    CASE "next": o$ = "GOSUB " + __Libsus_IncPtr
                    CASE "previous": o$ = "GOSUB " + __Libsus_DecPtr
                    CASE ELSE: GOTO SyntaxError
                END SELECT

            ELSE GOTO SyntaxError
            END IF


        CASE "say"
            '   say <label>
            REM GOTO LABEL_<labelno>

            IF RIGHT$(args$(1), 2) = "us" THEN
                o$ = "GOTO " + GetLabel(args$(1))
            ELSE GOTO SyntaxError
            END IF


        CASE "become"
            '   become the <label>
            REM L<labelno>:

            IF args$(1) = "the" THEN
                IF RIGHT$(args$(2), 2) = "us" THEN
                    o$ = GetLabel(args$(2)) + ":"
                ELSE GOTO SyntaxError
                END IF
            ELSE GOTO SyntaxError
            END IF


        CASE "is"
            '   is impostor sus?
            REM GOSUB Ask

            IF args$(1) = "imposter" AND _
               args$(2) = "sus?" THEN
                o$ = "GOSUB " + __Libsus_Input
            END IF


        CASE ELSE
            SyntaxError:
            Log "warn", "Syntax error. Ignoring"

    END SELECT

    TranslateCommand = o$
END FUNCTION




FUNCTION GetLabel$ (Label$) 'fetch label from label db
    FOR i~& = LBOUND(Labels) TO UBOUND(Labels)
        IF Labels(i~&) = Label$ THEN GetLabel = "L" + LTRIM$(STR$(i~&)): EXIT FUNCTION
    NEXT
    GetLabel = MakeLabel(Label$)
END FUNCTION




FUNCTION MakeLabel$ (Label$) 'add label to label db
    REDIM _PRESERVE Labels(UBOUND(labels) + 1) AS STRING
    Labels(UBOUND(labels)) = Label$
    MakeLabel = "L" + LTRIM$(STR$(UBOUND(labels)))
END FUNCTION




SUB Log (lv$, s$)
    SELECT CASE lv$
        CASE "warn"
            IF __Log_Suppress_Warn THEN EXIT SUB
            COLOR __Log_Color_Warn
            p$ = "WARNING"
            GOSUB CompilerError

        CASE "i warn"
            IF __Log_Suppress_Warn THEN EXIT SUB
            COLOR __Log_Color_Warn
            p$ = "WARNING"
            GOSUB NormalError

        CASE "error"
            IF __Log_Suppress_Error THEN EXIT SUB
            COLOR __Log_Color_Error
            p$ = "ERROR"
            GOSUB CompilerError

        CASE "i error"
            IF __Log_Suppress_Error THEN EXIT SUB
            COLOR __Log_Color_Error
            p$ = "INTERNAL ERROR"
            GOSUB NormalError

        CASE "fatal"
            IF __Log_Suppress_Fatal THEN EXIT SUB
            COLOR __Log_Color_Fatal
            p$ = "FATAL"
            GOSUB CompilerError

        CASE "i fatal"
            IF __Log_Suppress_Fatal THEN EXIT SUB
            COLOR __Log_Color_Fatal
            p$ = "FATAL"
            GOSUB NormalError

        CASE "info"
            IF __Log_Suppress_Info THEN EXIT SUB
            COLOR __Log_Color_Info
            p$ = "INFO"
            GOSUB NormalError

        CASE "i debug", "debug"
            IF __Log_Suppress_Debug THEN EXIT SUB
            COLOR __Log_Color_Debug
            p$ = "DEBUG"
            GOSUB CompilerError
    END SELECT
    EXIT SUB

    CompilerError:
    IF __LogFile THEN
        PRINT #__LogFile, p$; ": On line"; CurrentLineNo; " ("; CurrentLine; "):"
        PRINT #__LogFile, s$
    END IF

    IF __Log_Off THEN RETURN

    PRINT p$;
    COLOR __Log_Color_Normal
    IF CurrentLine = "" THEN _
        PRINT ": On line"; CurrentLineNo; ":" _
    ELSE _
        PRINT ": On line"; CurrentLineNo; " (" + CurrentLine + "):"
    PRINT s$
    RETURN


    NormalError:
    IF __LogFile THEN PRINT #__LogFile, p$; ": "; s$

    IF __Log_Off THEN RETURN

    PRINT p$;
    COLOR __Log_Color_Normal
    PRINT ": "; s$
    RETURN

END SUB


'FUNCTION GenLibsus$
'    o$ = "'begin libsus" + NL
'    o$ = o$ + "$CONSOLE:ONLY" + NL
'    o$ = o$ + "'$DYNAMIC" + NL
'    o$ = o$ + "_DEST _CONSOLE:"
'    o$ = o$ + "DIM A(0)AS _BIT,P AS _INTEGER64:"
'    o$ = o$ + "ON ERROR GOTO E:"
'    o$ = o$ + "GOTO B"
'    o$ = o$ + NL
'    o$ = o$ + "E:"
'    o$ = o$ + "RESUME NEXT"
'    o$ = o$ + NL
'    o$ = o$ + "IP:"
'    o$ = o$ + "P=P+1:"
'    o$ = o$ + "IF P>UBOUND(A)THEN REDIM _PRESERVE A(LBOUND(A) TO P)AS _BIT" + NL
'    o$ = o$ + "RETURN"
'    o$ = o$ + NL
'    o$ = o$ + "DP:"
'    o$ = o$ + "P=P-1:"
'    o$ = o$ + "IF P<LBOUND(A)THEN REDIM _PRESERVE A(P TO UBOUND(A))AS _BIT" + NL
'    o$ = o$ + "RETURN"
'    o$ = o$ + NL
'    o$ = o$ + "I:"
'    o$ = o$ + "INPUT" + CHR$(34) + CHR$(34) + ",A$:"
'    o$ = o$ + "SELECT CASE LCASE$(A$):"
'    o$ = o$ + "CASE" + CHR$(34) + "sus" + CHR$(34) + ":A(P)=1:"
'    o$ = o$ + "CASE" + CHR$(34) + "not sus" + CHR$(34) + ":A(P)=0:"
'    o$ = o$ + "CASE ELSE:?" + CHR$(34) + "Invalid susness you sussy baka" + CHR$(34) + ":GOTO I:"
'    o$ = o$ + "END SELECT:"
'    o$ = o$ + "RETURN" + NL
'    o$ = o$ + "B:" + NL
'    o$ = o$ + "'end libsus"
'    GenLibsus = o$
'END FUNCTION
