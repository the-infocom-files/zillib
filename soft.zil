"SOFT for
				 Library
	(c) Copyright 1988 Infocom, Inc. All Rights Reserved."

"This file defines everything needed to have soft function key definitions.
You must define a segment named SOFT-KEYS, and you may define your own max
function key definition length and window to use. You should invoke SOFT-KEYS
in some later file to define your initial function key definitions, if you
don't like the blank defaults."

<DEFAULTS-DEFINED READ-CHAR	;"routine to read one character"
		  READ-LINE	;"routine to read a line"
		  MOUSE-INPUT	;"routine to handle mouse input"
		  FKEY-MAX-LEN	;"max length of a function key def"
		  SOFT-WINDOW	;"number of window to use for v-define"
		  SOFT-KEY-DEFINITIONS ;"actual key defs"
		  DIROUT-TABLE>

<BEGIN-SEGMENT 0>

<CONSTANT TCHARS <TABLE (BYTE) 255 0>>

<GLOBAL DEMO-VERSION? <>>
;<CONSTANT INPUT-DEMO <>>

<CONSTANT SLIDE-SHOW-TIMEOUT 150>
<CONSTANT DEMO-TIMEOUT 600>

<ROUTINE SLIDE-SHOW-HANDLER ()
	<SETG DEMO-VERSION? +1>
	<RTRUE>>

<ROUTINE READ-DEMO (ARG1 "OPT" (ARG2 <>) "AUX" CHR)
	 <SETG DEMO-VERSION? -1>
	 <SET CHR <READ .ARG1 .ARG2 ,DEMO-TIMEOUT ,SLIDE-SHOW-HANDLER>>
	 <COND (<1? ,DEMO-VERSION?>
		<END-DEMO>)
	       (T
		<WINPUT ,S-TEXT 15 -999> ;"Disable MORE counter."
		.CHR)>>

<ROUTINE INPUT-DEMO (ARG "AUX" CHR)
	 <SETG DEMO-VERSION? -1>
	 <SET CHR <INPUT .ARG ,DEMO-TIMEOUT ,SLIDE-SHOW-HANDLER>>
	 <COND (<1? ,DEMO-VERSION?>
		<END-DEMO>)
	       (T
		<WINPUT ,S-TEXT 15 -999> ;"Disable MORE counter."
		.CHR)>>

"redefinition of default READ-INPUT routine; you must put a DELAY-DEFINITION
in your DEFS file"

<DEFAULT-DEFINITION READ-CHAR
<DEFMAC READ-CHAR ()
   '<COND (,DEMO-VERSION? <INPUT-DEMO 1>)
	  (T <INPUT 1>)>>>

<DEFAULT-DEFINITION READ-LINE
<DEFMAC READ-LINE ()
   '<COND (,DEMO-VERSION? <READ-DEMO ,P-INBUF>)
	  (T <READ ,P-INBUF <>>)>>>

<ROUTINE READ-INPUT ("AUX" TRM TMP FDEF)
	 <TELL ">">
	 <PUTB ,P-INBUF 1 0>
	 <REPEAT ()
		 <SET TRM <READ-LINE>>
		 <SET TRM <CONVERT-KEYS .TRM>>
		 <COND (<EQUAL? .TRM 13 10> <RETURN>)
		       (<EQUAL? .TRM ,CLICK1 ,CLICK2>
			<COND (<MOUSE-INPUT .TRM>
			       <RETURN>)
			      (ELSE <SOUND ,S-BEEP>)>)
		       (<AND <SET TMP
				  <INTBL? .TRM <REST ,FKEYS 2> <GET ,FKEYS 0>>>
			     <SET FDEF <GET .TMP 1>> ;"key def"
			     <GETB .FDEF 1>> ;"number chars in def"
			<SET TRM <ADD-TO-INPUT <REST .FDEF> .TRM>>
			<COND (<EQUAL? .TRM 13 10> <RETURN>)>)
		       (ELSE <SOUND ,S-BEEP>)>>
	 <SCRIPT-INBUF>
	 <LEX ,P-INBUF ,P-LEXV>>

"converts keypad keys to function keys"

<ROUTINE CONVERT-KEYS (TRM)
	 <COND (<EQUAL? .TRM ,PAD0>
		<SET TRM ,F10>)
	       (<AND <G=? .TRM ,PAD1>
		     <L=? .TRM ,PAD9>>
		<SET TRM <+ ,F1 <- .TRM ,PAD1>>>)>
	 .TRM>

"mouse input handler, takes term char (click1 or click2).
if it returns non-false, input was handled, otherwise read-input beeps"

<GLOBAL MOUSE-LOC-X <>>
<GLOBAL MOUSE-LOC-Y <>>

<DEFAULT-DEFINITION MOUSE-INPUT
	<ROUTINE MOUSE-INPUT (TRM)
		 <SETG MOUSE-LOC-X <LOWCORE MSLOCX>>
		 <SETG MOUSE-LOC-Y <LOWCORE MSLOCY>>
		 <APPLY <GETP ,HERE ,P?MOUSE> .TRM>>>

"call with ltable of bytes, terminating character"

<ROUTINE ADD-TO-INPUT (FDEF TRM "AUX" (M <GETB .FDEF 0>) N TMP)
	 <SET N <GETB ,P-INBUF 1>> ;"number chars already"
	 <COND (<EQUAL? <GETB .FDEF .M> 13 10>
		<SET TRM 13> ;"this def is a terminator"
		<SET M <- .M 1>>)>
	 <SET TMP <REST ,P-INBUF <+ .N 2>>>
	 <COND (<G=? <+ .M .N>
		     <- <GETB ,P-INBUF 0> 1>> ;"overflowed input buffer"
		<SOUND 1>
		<SET M <- <GETB ,P-INBUF 0> .N 2>>)>
	 <SET FDEF <REST .FDEF>> ;"skip the count byte"
	 <COPYT .FDEF .TMP .M>
	 <PUTB .TMP .M 0>
	 <WINATTR -3 ,A-SCRIPT ,O-CLEAR>
	 <PRINTT .FDEF .M>
	 <PUTB ,P-INBUF 1 <+ .N .M>>
	 <COND (<EQUAL? .TRM 13 10>
		<CRLF>)>
	 <WINATTR -3 ,A-SCRIPT ,O-SET>
	 .TRM>

"scripts input buffer once input line is terminated"

<ROUTINE SCRIPT-INBUF ("AUX" BUF (CNT 0) (N <GETB ,P-INBUF 1>) CHR)
	 <DIROUT ,D-SCREEN-OFF>
	 <SET BUF <REST ,P-INBUF>>
	 <REPEAT ()
		 <COND (<IGRTR? CNT .N> <RETURN>)
		       (ELSE
			<SET CHR <GETB .BUF .CNT>> 
			<COND (<AND <G=? .CHR !\a>
				    <L=? .CHR !\z>>
			       <PRINTC <- .CHR 32>>)
			      (ELSE <PRINTC .CHR>)>)>>
	 <CRLF>
	 <DIROUT ,D-SCREEN-ON>>

<ROUTINE YES? ("AUX" TMP)
	 <REPEAT ()
		 <PRINTI "?|
(Y is affirmative): >">
		 <SET TMP <CONVERT-KEYS <READ-CHAR>>>
		 <COND (<NOT <EQUAL? .TMP 13 10>> <CRLF>)>
		 <COND (<EQUAL? .TMP !\Y !\y ,CLICK1 ,CLICK2>
			<RTRUE>)
		       (<EQUAL? .TMP !\N !\n>
			<RFALSE>)>>>

<DEFAULT-DEFINITION DIROUT-TABLE
	<CONSTANT DIROUT-TABLE <ITABLE 80 <BYTE 0>>>>

<ROUTINE PRINT-CENTER-TABLE ("OPT" (TMAX <>) "AUX" L)
	 <DIROUT ,D-TABLE-OFF>
	 <SET L <LOWCORE TWID>>
	 <COND (.TMAX
		<CURSET <WINGET -3 ,WYPOS>
			<+ </ <- <WINGET -3 ,WWIDE>
				 .TMAX>
			      2>
			   1>>
		<XERASE .TMAX>)>
	 <CURSET <WINGET -3 ,WYPOS>
		 <+ </ <- <WINGET -3 ,WWIDE>
			  .L>
		       2>
		    1>>
	 <PRINTT <REST ,DIROUT-TABLE 2> <GET ,DIROUT-TABLE 0>>>

<GLOBAL SPACE-WIDTH 0> ;"used to save the width of one space"
<CONSTANT SPACE-TABLE <LTABLE (STRING) "          ">>

"substitute for erase that works on the amiga"

<ROUTINE XERASE (N "AUX" X)
	 <COND (<EQUAL? ,MACHINE ,AMIGA>
		<COND (<NOT ,SPACE-WIDTH>
		       <DIROUT ,D-TABLE-ON ,DIROUT-TABLE>
		       <PRINTI " ">
		       <DIROUT ,D-TABLE-OFF>
		       <SETG SPACE-WIDTH <LOWCORE TWID>>)>
		<COND (<EQUAL? .N 1>
		       <SET N <- <WINGET -3 ,WWIDE>
				 <WINGET -3 ,WXPOS>
				 1>>)>
		<SET X <MOD .N ,SPACE-WIDTH>>
		<SET N </ .N ,SPACE-WIDTH>>
		<COND (.X <SET N <+ .N 1>>)>
		<REPEAT ((L <GETB ,SPACE-TABLE 0>)
			 (OX <WINGET -3 ,WXPOS>)
			 (OY <WINGET -3 ,WYPOS>))
		   <COND (<L? .N .L>
			  <PRINT-TABLE ,SPACE-TABLE .N>
			  <CURSET .OY .OX>
			  <RETURN>)
			 (ELSE
			  <SET N <- .N .L>>
			  <PRINT-TABLE ,SPACE-TABLE .L>)>>)
	       (ELSE <ERASE .N>)>>

"color inverter that works on amiga"

<ROUTINE INVERSE-COLOR ()
	 <COND (<EQUAL? ,MACHINE ,AMIGA>
		<HLIGHT ,H-INVERSE>)
	       (ELSE <COLOR ,BG-COLOR ,FG-COLOR>)>>

"color normalizer that works on amiga"

<ROUTINE NORMAL-COLOR ()
	 <COND (<EQUAL? ,MACHINE ,AMIGA>
		<HLIGHT ,H-NORMAL>)>
	 <COLOR ,FG-COLOR ,BG-COLOR>>

<ROUTINE TYPE-ANY-KEY ("AUX" KEY)
	 <WINATTR -3 ,A-SCRIPT ,O-CLEAR>
	 <TELL "[Type any key to continue]">
	 <SET KEY <READ-CHAR>>
	 <WINATTR -3 ,A-SCRIPT ,O-SET>
	 .KEY>

<END-SEGMENT ;"0">



"soft key definitions"

<BEGIN-SEGMENT SOFT-KEYS>

"MDL routine to create a set of soft-key tables and defaults"

<DEFINE SOFT-KEYS ("ARGS" TUP "AUX" (CNT 0) (DL (0)) L)
	<SET L
	<MAPF ,LIST
	      <FUNCTION ("AUX" VAL STR STRL)
		   <COND (<EMPTY? .TUP> <MAPSTOP>)>
		   <SET VAL <NTH .TUP 1>>
		   <SET STR <NTH .TUP 2>>
		   <SET TUP <REST .TUP 2>>
		   <COND (<TYPE? .STR STRING>
			  <SET STRL <LENGTH .STR>>
			  <SET DL (.VAL .STRL .STR !.DL)>
			  <COND (<L? .STRL ,FKEY-MAX-LEN>
				 <SET STR
				      <STRING .STR
					      <ISTRING <- ,FKEY-MAX-LEN .STRL>
						       !\ >>>)>
			  <SET CNT <+ .CNT 1>>
			  <MAPRET .VAL <TABLE (STRING) ,FKEY-MAX-LEN .STRL .STR>>)
			 (ELSE
			  <MAPRET .VAL <EVAL .STR>>)>>>>
	<CONSTANT FKEY-TBL <NTH .L 2>>
	<CONSTANT FKEYS-STRTABLE-LEN <* .CNT <+ ,FKEY-MAX-LEN 2>>>
	<CONSTANT DEFAULT-FKEYS <TABLE (STRING) !.DL>>
	<CONSTANT FKEYS <LTABLE !.L>>>

<CONSTANT FNAMES
	  <LTABLE ,UP-ARROW " UP"
		  ,DOWN-ARROW " DN"
		  ,LEFT-ARROW " LF"
		  ,RIGHT-ARROW " RT"
		;"vt100 keypad keys"
		  ,F1 " F1" 
		  ,F2 " F2"
		  ,F3 " F3"
		  ,F4 " F4"
		  ,F5 " F5"
		  ,F6 " F6"
		  ,F7 " F7"
		  ,F8 " F8"
		  ,F9 " F9"
		  ,F10 "F10">>

<SYNTAX DEFINE = V-DEFINE>

<GLOBAL DONE-DEFINE? <>>

<GLOBAL MONO-X 0>

<ROUTINE V-DEFINE ("AUX" (LINE 0) LINMAX CHR TMP NLINE FKEY FDEF LEFT ;FX ;FY)
	 <COND (<APPLE?> <PICINF ,P-SOFT-KEY-SEG ,YX-TBL>)>
	 <COND (<NOT ,DONE-DEFINE?>
		<TELL
"Use the up-arrow and down-arrow keys">
		<COND (<FLAG-ON? ,F-MOUSE>
		       <TELL " or mouse">)>	
		<TELL
" to move to the function key or operation you want. Double-click or
carriage-return to perform operations." CR>
		<TYPE-ANY-KEY>
		<COND (<NOT ,DONE-DEFINE?>
		       <SETG DONE-DEFINE? <MAX-SOFT-CMD>>)>)>
	 <CLEAR -1>
	 <SET FKEY <REST ,FKEYS <+ 2 <* 4 .LINE>>>>
	 <SET FDEF <GET .FKEY 1>>
	 <SET LEFT </ <- <LOWCORE SCRH> <GETB .FDEF 0>> 2>>
	 <SET LINMAX </ <GET ,FKEYS 0> 2>>
	 <SCREEN ,SOFT-WINDOW>
	 <FONT 4>
	 <SET TMP <WINGET ,SOFT-WINDOW ,WFSIZE>>
	 <SETG MONO-X <LOW-BYTE .TMP>>
	 <WINPOS ,SOFT-WINDOW
		 <* ,FONT-Y </ <- <LOWCORE SCRV> .LINMAX> 2>>
		 <* ,MONO-X .LEFT>>
	 <WINSIZE ,SOFT-WINDOW
		  <* ,FONT-Y <+ .LINMAX 1>>
		  <+ 1 <* ,MONO-X <+ ,FKEY-MAX-LEN 4>>>>
	 <DISPLAY-SOFTS .LINE>
	 <DISPLAY-SOFT .FKEY .LINE <>>
	 <REPEAT ()
		 <SET CHR <CONVERT-KEYS <READ-CHAR>>>
		 <SET NLINE .LINE>
		 <COND (<AND <EQUAL? .CHR ,CLICK1 ,CLICK2>
			     <SET TMP <IN-WINDOW? ,SOFT-WINDOW>>
			     <G? .TMP 1>>
			<SET NLINE <- .TMP 2>>
			<COND (<NOT <EQUAL? .LINE .NLINE>>
			       <COND (<ZERO? <GET ,FKEYS <+ 2 <* 2 .NLINE>>>>
				      <SET NLINE .LINE>
				      <SOUND ,S-BEEP>)
				     (ELSE
				      <DISPLAY-SOFT .FKEY .LINE T>
				      <DISPLAY-SOFT <REST ,FKEYS
							  <+ 2 <* 4 .NLINE>>>
						    .NLINE
						    <>>
				      <SET LINE .NLINE>
				      <SET FKEY <REST ,FKEYS <+ 2 <* 4 .LINE>>>>
				      <SET FDEF <GET .FKEY 1>>)>)>
			<COND (<AND <EQUAL? .CHR ,CLICK2>
				    <L? <GET .FKEY 0> 0>>
			       <SET CHR 13>)>)>
		 <COND (<EQUAL? .CHR ,CLICK1 ,CLICK2>)
		       (<AND <EQUAL? .CHR 13>
			     <L? <GET .FKEY 0> 0>>
			<SET NLINE 0>
			<COND (<APPLY <GET .FDEF 1>>
			       <RETURN>)
			      (ELSE
			       <SET NLINE <- .LINMAX 1>>
			       <DISPLAY-SOFTS .LINE>)>)
		       (<EQUAL? .CHR ,DOWN-ARROW 13>
			<COND (<L? <SET NLINE <+ .NLINE 1>> .LINMAX>
			       <COND (<ZERO? <GET ,FKEYS <+ 2 <* 2 .NLINE>>>>
				      <SET NLINE <+ .NLINE 1>>)>)
			      (ELSE <SET NLINE 0>)>)
		       (<EQUAL? .CHR ,UP-ARROW>
			<COND (<G=? <SET NLINE <- .NLINE 1>> 0>
			       <COND (<ZERO? <GET ,FKEYS <+ 2 <* 2 .NLINE>>>>
				      <SET NLINE <- .NLINE 1>>)>)
			      (ELSE
			       <SET NLINE <- .LINMAX 1>>)>)
		       (<SET TMP
			     <INTBL? .CHR <REST ,FKEYS 2> <GET ,FKEYS 0>>>
			<SET NLINE </ <- .TMP ,FKEYS> 4>>)
		       (<G? <GET .FKEY 0> 0>
			<COND (<EQUAL? .CHR 8 127>
			       <SET TMP <GETB .FDEF 1>>
			       <COND (<NOT <ZERO? .TMP>>
				      <SET TMP <- .TMP 1>>
				      <PUTB .FDEF 1 .TMP>
				      <PUTB .FDEF <+ .TMP 2> !\ >
				      <CURSET <+ 1 <* <+ .LINE 1> ,FONT-Y>>
					      <+ 1 <* <+ .TMP 4> ,MONO-X>>>
				      <XERASE 1>)
				     (ELSE <SOUND ,S-BEEP>)>)
			      (<AND <G=? .CHR !\ > <L? .CHR 127>>
			       <SET TMP <GETB .FDEF 1>>
			       <COND (<EQUAL? .TMP <GETB .FDEF 0>>
				      <SOUND ,S-BEEP>)
				     (<SET LEFT
					   <INTBL? 13
					      <REST .FDEF 2>
					      <GETB .FDEF 1>
					      1>>
				      <SOUND ,S-BEEP>)
				     (ELSE
				      <COND (<EQUAL? .CHR !\| !\!> <SET CHR 13>)>
				      <PUTB .FDEF 1 <+ .TMP 1>>
				      <COND (<AND <G=? .CHR !\A>
						  <L=? .CHR !\Z>>
					     <SET CHR <+ .CHR 32>>)>
				      <PUTB .FDEF <+ .TMP 2> .CHR>
				      <COND (<EQUAL? .CHR 13>
					     <PRINTC !\|>)
					    (ELSE <PRINTC .CHR>)>)>)
			      (ELSE <SOUND ,S-BEEP>)>)
		       (ELSE <SOUND ,S-BEEP>)>
		 <COND (<NOT <EQUAL? .LINE .NLINE>>
			<DISPLAY-SOFT .FKEY .LINE T>
			<DISPLAY-SOFT <REST ,FKEYS <+ 2 <* 4 .NLINE>>>
				      .NLINE <>>
			<SET LINE .NLINE>
			<SET FKEY <REST ,FKEYS <+ 2 <* 4 .LINE>>>>
			<SET FDEF <GET .FKEY 1>>)>>
	 <FONT 1>
	 <SCREEN ,S-TEXT>
	 <CLEAR ,S-TEXT>
	 <V-$REFRESH>>

"given a window, returns line hit with mouse click, or false if not in that
window."

<ROUTINE IN-WINDOW? (W "AUX" X Y TOP LEFT)
	 <SET Y <LOWCORE MSLOCY>>
	 <SET X <LOWCORE MSLOCX>>
	 <COND (<OR <L? .Y <SET TOP <WINGET .W ,WTOP>>>
		    <L? .X <SET LEFT <WINGET .W ,WLEFT>>>>
		<RFALSE>)
	       (ELSE
		<SET Y <- .Y .TOP>>
		<SET X <- .X .LEFT>>
		<COND (<OR <G? .Y <WINGET .W ,WHIGH>>
			   <G? .X <WINGET .W ,WWIDE>>>
		       <RFALSE>)>
		<SET Y <+ 1 </ .Y ,FONT-Y>>>
		<RETURN .Y>)>>

<ROUTINE MAX-SOFT-CMD ("AUX" (L <GET ,FKEYS 0>) FKEY (CNT 0) (TMAX 0) FDEF)
	 <SET L </ .L 2>>
	 <SET FKEY <REST ,FKEYS 2>>
	 <REPEAT ()
		 <COND (<L? .CNT .L>
			<COND (<AND <L? <GET .FKEY 0> 0>
				    <NOT <ZERO? <SET FDEF <GET .FKEY 1>>>>>
			       <DIROUT ,D-TABLE-ON ,DIROUT-TABLE>
			       <TELL "  " <GET .FDEF 0> "  ">
			       <DIROUT ,D-TABLE-OFF>
			       <COND (<G? <LOWCORE TWID> .TMAX>
				      <SET TMAX <LOWCORE TWID>>)>)>
			<SET FKEY <REST .FKEY 4>>)
		       (ELSE
			<RETURN .TMAX>)>
		 <SET CNT <+ .CNT 1>>>>

<ROUTINE DISPLAY-SOFTS (LINE "AUX" (L <GET ,FKEYS 0>) (F 0) N FKEY (CNT 0))
	 <SET L </ .L 2>>
	 <SCREEN ,SOFT-WINDOW>
	 <CURSET 1 1>
	 <DIROUT ,D-TABLE-ON ,DIROUT-TABLE>
	 <FONT 1>
	 <TELL "Function Keys">
	 <PRINT-CENTER-TABLE>
	 <FONT 4>
	 <SET FKEY <REST ,FKEYS 2>>
	 <REPEAT ()
		 <COND (<L? .CNT .L>
			<DISPLAY-SOFT .FKEY .CNT
				      <COND (<EQUAL? .CNT .LINE> <>)
					    (ELSE T)>>
			<SET FKEY <REST .FKEY 4>>)
		       (ELSE <RETURN>)>
		 <SET CNT <+ .CNT 1>>>>

<ROUTINE DISPLAY-SOFT (FKEY CNT INV?
		       "AUX" (FDEF <GET .FKEY 1>) S N M TMP
		       (Y <+ .CNT 2>) X)
	 <COND (<L? <GET .FKEY 0> 0> ;"constant string"
		<COND (<NOT <ZERO? <GET .FKEY 1>>>
		       <CCURSET .Y 1>
		       <COND (.INV?
			      <INVERSE-COLOR>)>
		       <FONT 1>
		       <DIROUT ,D-TABLE-ON ,DIROUT-TABLE>
		       <TELL <GET .FDEF 0>>
		       <PRINT-CENTER-TABLE ,DONE-DEFINE?>
		       <FONT 4>)>)
	       (ELSE
		<SET S <GETB .FDEF 0>>
		<SET N <GETB .FDEF 1>>
		<CCURSET .Y 1>
		<COND (<SET TMP
			    <INTBL? <GET .FKEY 0>
				    <REST ,FNAMES 2>
				    <GET ,FNAMES 0>>>
		       <COND (.INV? <NORMAL-COLOR>)
			     (ELSE <INVERSE-COLOR>)>
		       <TELL <GET .TMP 1>>
		       <NORMAL-COLOR>
		       <TELL " ">
		       <COND (.INV?
			      <INVERSE-COLOR>)
			     (ELSE
			      <NORMAL-COLOR>)>)>
		<SET FDEF <REST .FDEF 2>> ;"get past header bytes"
		<COND (.N ;"any definition?"
		       <SET M <- .N 1>>
		       <COND (<EQUAL? <GETB .FDEF .M> 13> ;"last character CR?"
			      <PRINTT .FDEF .M>
			      <PRINTC !\|>
			      <SET FDEF <REST .FDEF .N>>
			      <SET S <- .S .N>>)>)>
		<PRINTT .FDEF .S>
		<XERASE 1> ;"clear to eol"
		<COND (<NOT .INV?>
		       <CURSET <+ 1 <* <- .Y 1> ,FONT-Y>>
			       <+ 1 <* <+ .N 4> ,MONO-X>>>)>)>
	 <NORMAL-COLOR>>

<ROUTINE SOFT-RESET-DEFAULTS ("AUX" K L KEYS DEF KL TMP)
	 <SET KL <GET ,FKEYS 0>>
	 <SET DEF ,DEFAULT-FKEYS>
	 <REPEAT ()
		 <SET K <GETB .DEF 0>>
		 <COND (<ZERO? .K> <RETURN>)>
		 <SET DEF <REST .DEF>>
		 <SET L <+ 1 <GETB .DEF 0>>>
		 <COND (<SET KEYS <INTBL? .K <REST ,FKEYS 2> .KL>>
			<SET KEYS <GET .KEYS 1>>
			<SET TMP <REST .KEYS>>
			<PUTB .TMP 0 !\ >
			<COPYT .TMP <REST .TMP> <- <GETB .KEYS 0>>>
			<COPYT .DEF <REST .KEYS> .L>)>
		 <SET DEF <REST .DEF .L>>>
	 <RFALSE>>

<CONSTANT DEFS-NAME <LTABLE (PURE STRING) "DEFS">>

<ROUTINE SOFT-SAVE-DEFS ()
	 <CLEAR ,S-TEXT>
	 <SCREEN ,S-TEXT>
	 <COND (<NOT <SAVE ,FKEY-TBL
			   ,FKEYS-STRTABLE-LEN
			   ,DEFS-NAME>>
	        <TELL "Failed.">)>
	 <CLEAR ,S-TEXT>
	 <SCREEN ,SOFT-WINDOW>
	 <RFALSE>>

<ROUTINE SOFT-RESTORE-DEFS ()
	 <CLEAR ,S-TEXT>
	 <SCREEN ,S-TEXT>
	 <COND (<NOT <RESTORE ,FKEY-TBL ,FKEYS-STRTABLE-LEN ,DEFS-NAME>>
		<TELL "Failed.">)>
	 <CLEAR ,S-TEXT>
	 <SCREEN ,SOFT-WINDOW>
	 <RFALSE>>

<ROUTINE SOFT-EXIT ()
	 <RTRUE>>

<DEFAULT-DEFINITION FKEY-MAX-LEN
	 <CONSTANT FKEY-MAX-LEN 30>>	;"max length of a key definition"
<DEFAULT-DEFINITION SOFT-WINDOW
	 <CONSTANT SOFT-WINDOW 2>>	;"window to use for defining"

"table containing string definitions for each function key.
contiguous so it can be written out or read in."

<DEFAULT-DEFINITION SOFT-KEY-DEFINITIONS
	 <SOFT-KEYS
	   ,UP-ARROW ""
	   ,DOWN-ARROW ""
	   ,LEFT-ARROW ""
	   ,RIGHT-ARROW ""
	   ;"vt100 keypad keys"
	   ,F1 "" 
	   ,F2 ""
	   ,F3 ""
	   ,F4 ""
	   ,F5 ""
	   ,F6 ""
	   ,F7 ""
	   ,F8 ""
	   ,F9 ""
	   ,F10 ""
	   -1 0
	   -1 <TABLE "Save Definition File" SOFT-SAVE-DEFS>
	   -1 <TABLE "Restore Definition File" SOFT-RESTORE-DEFS>
	   -1 <TABLE "Reset Defaults" SOFT-RESET-DEFAULTS>
	   -1 <TABLE "Exit" SOFT-EXIT>>>

<END-SEGMENT ;"SOFT-KEYS">