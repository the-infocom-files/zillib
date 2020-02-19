"CLUES for LIBRARY
(c) Copyright 1988 Infocom, Inc.  All Rights Reserved."

"To install:
Add <XFLOAD <ZILLIB>CLUES> to your GAME.ZIL file.
Modify ROUTINE FINISH in VERBS to include Hint.
Add HINT syntaxes (be careful -- you might already have some variety).
Add verb routines for V-HINT and V-HINTS-OFF.
Add HINT to list of verbs in CLOCKER-VERB (a.k.a. GAME-VERB?).
Make sure flag in V-HINTS-OFF syntax is correct (RLANDBIT, KLUDGEBIT, etc.)."

<FILE-FLAGS CLEAN-STACK? MDL-ZIL? ;ZAP-TO-SOURCE-DIRECTORY?>

<COMPILATION-FLAG-DEFAULT SEGMENTED-HINTS <>>
<COMPILATION-FLAG-DEFAULT SPLIT-HINTS <>>
<COMPILATION-FLAG-DEFAULT SELECTIVE-HINTS <>>

<DEFAULTS-DEFINED
	HINT-TITLES
	INIT-HINT-SCREEN
	LEAVE-HINT-SCREEN>

<IFFLAG (SEGMENTED-HINTS
	 <BEGIN-SEGMENT 0>)
	(ELSE
	 <BEGIN-SEGMENT HINTS>)>

<DEFAULT-DEFINITION HINT-TITLES
	<CONSTANT RETURN-SEE-HINT	;"Hit " "Return for hints.">
	<CONSTANT RETURN-SEE-NEW-HINT	;"Hit " "Return for a hint.">
	<CONSTANT M-MAIN-HINT-MENU	;"Hit " "M for main menu.">
	<CONSTANT M-SEE-HINT-MENU	;"Hit " "M for hint menu.">
	<CONSTANT PREVIOUS-HINT		;"Hit " "P for previous item.">
	<CONSTANT NEXT-HINT		;"Hit " "N for next item.">
	<CONSTANT Q-RESUME-STORY	;"Hit " "Q to resume story.">
	<CONSTANT H-OR-USE-MOUSE	"(Or use mouse.)">
	<GLOBAL H-OR-USE-MOUSE-WID:NUMBER 0>
	<CONSTANT NO-MORE-HINTS		"[No more hints.]">>

<END-SEGMENT>

<IFFLAG (SEGMENTED-HINTS
	 <BEGIN-SEGMENT 0>)
	(SPLIT-HINTS
	 <BEGIN-SEGMENT CLUES>
	 <COND (<GASSIGNED? DISTINGUISHED-SEGMENT>
		<DISTINGUISHED-SEGMENT HINTS>
		<DISTINGUISHED-SEGMENT CLUES>)>)
	(T
	 <COND (<GASSIGNED? DISTINGUISHED-SEGMENT>
		<DISTINGUISHED-SEGMENT HINTS>)>
	 <BEGIN-SEGMENT HINTS>)>

<DEFINE20 EVAL-INTO-HINTS? ()
  <COND (<COMPILATION-FLAG-VALUE "SEGMENTED-HINTS"> <>)
	(<COMPILATION-FLAG-VALUE "SPLIT-HINTS"> T)>>

"If the first argument is non-false, build a parallel impure table
   for storing the count of answers already seen; make it a constant
   under the given name."

<DEFINE20 CONSTRUCT-HINTS (COUNT-NAME "ARGS" STUFF "AUX" (SS <>)
			   END-STUFF VAL
			   (HL (T)) (HLL .HL) V
			   (CL (T)) (CLL .CL)
			   TCL TCLL MAIN-SEGMENT (CURRENT-SEGMENT <>))
   <IFFLAG (SEGMENTED-HINTS
	    <BIND ()
	       <SET MAIN-SEGMENT "0">
	       <SET CURRENT-SEGMENT "0">>)>
   <SET STUFF <LIST !.STUFF>>
   <REPEAT ((CT 0))
     <COND (<OR <EMPTY? .STUFF>
		<TYPE? <1 .STUFF> STRING>>
	    ;"Chapter break"
	    <SET END-STUFF .STUFF>
	    <IFFLAG (SEGMENTED-HINTS
		     <COND (<EMPTY? .STUFF>)
			   (<AND <NOT <EMPTY? <REST .STUFF>>>
				 <TYPE? <2 .STUFF> STRING>>
			    <SET CURRENT-SEGMENT <2 .STUFF>>
			    <2 .STUFF <1 .STUFF>>
			    <SET STUFF <REST .STUFF>>)
			   (T
			    <SET CURRENT-SEGMENT .MAIN-SEGMENT>)>)
		    (T
		     <COND (<EMPTY? .STUFF>)
			   (<AND <NOT <EMPTY? <REST .STUFF>>>
				 <TYPE? <2 .STUFF> STRING>>
			    <2 .STUFF <1 .STUFF>>
			    <SET STUFF <REST .STUFF>>)>)>
	    <COND
	     (<NOT .SS>
	      ;"First one, just do setup"
	      <SET SS .STUFF>
	      <SET TCL (T)>
	      <SET TCLL .TCL>
	      <SET CT 0>)
	     (T
	      <SET V <IVECTOR <- <LENGTH .SS> <LENGTH .END-STUFF>>>>
	      <MAPR <> 
		    <FUNCTION (L V) <1 .V <1 .L>>>
		    .SS .V>
	      ;<SET V <SUBSTRUC .SS 0 <- <LENGTH .SS> <LENGTH .STUFF>>>>
	      ;"One chapter's worth"
	      <COND (<L? 17 <LENGTH .V>>
		     <WARN!-ZILCH!-PACKAGE!- "Too many answers for: " <1 .V>>)>
	      <COND (<EVAL-INTO-HINTS?>
		     <SET HLL <REST
			<PUTREST .HLL
			 (<EVAL <FORM TABLE (LENGTH PURE SEGMENT
					     <IFFLAG (SPLIT-HINTS "CLUES")
						     (T "HINTS")>)
				      !.V>>)>>>)
		    (T
		     <SET HLL <REST
			<PUTREST .HLL
			 (<EVAL <FORM TABLE (LENGTH PURE)
				      !.V>>)>>>)>
	      <COND (.COUNT-NAME
		     <SET CLL <REST <PUTREST .CLL
					     (<EVAL <FORM TABLE (BYTE)
							  !<REST .TCL>>>)>>>
		     <SET TCL (T)>
		     <SET TCLL .TCL>
		     <SET CT 0>)>
	      <SET SS .STUFF>)>
	    <COND (<EMPTY? .STUFF> <RETURN>)>
	    <SET STUFF <REST .STUFF>>)
	   (T
	    <IFFLAG (SEGMENTED-HINTS
		     <1 .STUFF <EVAL-IN-SEGMENT .CURRENT-SEGMENT <1 .STUFF>>>)
		    (SPLIT-HINTS
		     <BIND ()
			<MAPR <>
			   <FUNCTION (X "AUX" (Y <1 .X>) TT)
			      <COND (<TYPE? .Y STRING>
				     <SET TT <EVAL-IN-SEGMENT "HINTS"
							      <FORM TABLE (TEMP-TABLE)
								    .Y>>>
				     <1 .X <ZGET .TT 0>>)
				    (<TYPE? .Y FORM>
				     <MAPR <>
					<FUNCTION (A "AUX" (B <1 .A>) TT)
					   <COND (<TYPE? .B STRING>
						  <SET TT <EVAL-IN-SEGMENT 
							   "HINTS"
							   <FORM TABLE (TEMP-TABLE)
								 .B>>>
						  <1 .A <ZGET .TT 0>>)>>
					.Y>)>>
			   <1 .STUFF>>
			<1 .STUFF <EVAL-IN-SEGMENT "CLUES" <1 .STUFF> T>>>)
		    (T
		     <1 .STUFF <EVAL-IN-SEGMENT "HINTS" <1 .STUFF> T>>)>
	    <COND (.COUNT-NAME
		   <COND (<1? <MOD <SET CT <+ .CT 1>> 2>>
			  <SET TCLL <REST <PUTREST .TCLL
						   (0)>>>)>)>
	    <SET STUFF <REST .STUFF>>)>>
   <COND (.COUNT-NAME
	  <COND (<EVAL-INTO-HINTS?>
		 <EVAL <FORM CONSTANT .COUNT-NAME
			       <EVAL <FORM TABLE (PURE SEGMENT
						  <IFFLAG (SPLIT-HINTS "CLUES")
							  (T "HINTS")>)
					   !<REST .CL>>>>>)
		(T
		 <EVAL <FORM CONSTANT .COUNT-NAME
		      <EVAL <FORM PTABLE !<REST .CL>>>>>)>)>
   <SET VAL <COND (<EVAL-INTO-HINTS?>
		   <EVAL <FORM TABLE (PURE LENGTH SEGMENT
				      <IFFLAG (SPLIT-HINTS "CLUES")
					      (T "HINTS")>) !<REST .HL>>>)
		  (T
		   <EVAL <FORM PLTABLE !<REST .HL>>>)>>
   <EVAL <FORM CONSTANT INVISICLUES " InvisiClues (tm)">>
   .VAL>

"Longest hint topic and longest question can be one line, unless it shares
a line with another such in the other column. Each question can have up to
16 answers but no more."

;<CONSTANT HINTS
  <CONSTRUCT-HINTS HINT-COUNTS ;"Put topics in Quotes - followed by PLTABLEs
				 of Questions and Answers in quotes"
	;"17 character wide"
	;"this set of quotes is 36 chars. wide"
	"Topic/Chapter"
	<PLTABLE "Question"
		 "Hint 1"
		 "Hint 2">>>

<GLOBAL H-QUEST-NUM 1>	"shows in HINTS LTABLE which question it's on"

<GLOBAL H-CHAPT-NUM 1>	"shows in HINTS LTABLE which chapter it's on"

<GLOBAL BOTTOM-HINT-NUM:NUMBER 0>
<CONSTANT TOP-HINT-LINE:NUMBER 1>
<CONSTANT LEFT-HINT-COLUMN:NUMBER 1>

<GLOBAL GET-HINT-ROUTINE 0>	"APPLY this to get pointer to text"

<DEFINE DO-HINTS ("AUX" Q WIN ;FCLR WCLR QN)
  <IF-SOUND <SETG SOUND-QUEUED? <>>
	    <KILL-SOUNDS>>
  <HLIGHT ,H-NORMAL>
  <CURSET -1 0> ;"turn cursor off"
  <SET WIN <INIT-HINT-SCREEN>>
  <WINATTR ,S-TEXT ,A-WRAP ,O-CLEAR>
  <FONT 1>
  <SET Q <SHIFT <WINGET -3 ,WFSIZE> -8>>	;"height"
  <SETG BOTTOM-HINT-NUM </ <WINGET -3 ,WHIGH> .Q> ;<- * 1>>
  <SCREEN ,S-WINDOW ;.WIN>
  <SET WCLR <WINGET ,S-WINDOW ;.WIN ,WCOLOR>>
  <COND (<EQUAL? <LOWCORE INTID> ,APPLE-2C ,APPLE-2E ,APPLE-2GS>
	 <COLOR 1 1>	;"Is this necessary?"
	 <HLIGHT ,H-INVERSE>)
	(<BAND 1 <LOWCORE ZVERSION>>	;"colors visible?"
	 <CCURSET 2 9>
	 <COLOR 1 -1>)
	(T		;"Flip colors from text screen."
	 <PROG (FG BG)
	    <SET BG <WINGET ,S-TEXT ,WCOLOR>>
	    <SET FG <SHIFT .BG -8>>
	    <COND (<EQUAL? .FG -1 *377*>	;"If weird pixel color, use standard."
		   <SET FG 1>)>
	    <SET BG <BAND .BG *377*>>
	    <COND (<EQUAL? .BG -1 *377*>
		   <SET BG 1>)>
	    <COLOR .FG .BG>>)>
 <PROG ()
  <CLEAR ,S-TEXT>
  <SCREEN ,S-WINDOW ;.WIN>
  <FONT 1>
  <HINT-TITLE ,INVISICLUES ,S-WINDOW ;.WIN>
  <SCREEN ,S-TEXT ;,S-FULL>
  <WINATTR -3 ,A-SCRIPT ,O-CLEAR>
  <IFFLAG (SELECTIVE-HINTS
	   <MOUSE-LIMIT -1>
	   <DIROUT ,D-TABLE-ON ,K-DIROUT-TBL>)
	  (T
	   <DIROUT ,D-TABLE-ON ,SLINE ;-80>)>
  <DIROUT ,D-TABLE-OFF>		;"flush TWID"
  <SETG GET-HINT-ROUTINE ,H-CHAPT-NAME>
  <H-PUT-UP-FROBS <GET ,HINTS 0>>
  <IFFLAG (SELECTIVE-HINTS
	   <COND (<ZERO? <SET QN <INTBL? ,H-CHAPT-NUM
				 <ZREST ,K-HINT-ITEMS 2>
				 <ZGET ,K-HINT-ITEMS 0>>>>
		  <SET QN 1>)
		 (T
		  <SET QN </ <- .QN ,K-HINT-ITEMS> 2>>)>)
	  (T
	   <SET QN ,H-CHAPT-NUM>)>
  <COND (<G? .QN <GET ,HINTS 0>>
	 <PRINTI " [">
	 <PRINTN .QN>
	 <PRINTI "]">
	 <SET QN 1>)>
  <H-NEW-CURSOR .QN>
  <REPEAT (CHR TMP NUM MAXC)
	  <IFFLAG (SELECTIVE-HINTS
		   <SET MAXC <ZGET ,K-HINT-ITEMS 0>>)
		  (T
		   <SET MAXC <ZGET ,HINTS 0>>)>
	  <COND (,DEMO-VERSION?
		 <SET CHR <INPUT-DEMO 1>>)
		(T
		 <SET CHR <INPUT 1>>)>
	  <COND (<EQUAL? .CHR ,CLICK1 ,CLICK2>
		 <SET TMP <SELECT-HINT-BY-MOUSE>>
		 <COND (<L=? .TMP 0>
			<COND (<EQUAL? .TMP -1>
			       <SET CHR !\N>)
			      (<EQUAL? .TMP -2>
			       <SET CHR !\P>)
			      (<EQUAL? .TMP -3>
			       <SET CHR 13>)
			      (<EQUAL? .TMP -4>
			       <SET CHR !\Q>)
			      (T
			       <SOUND ,S-BEEP>
			       <AGAIN>)>)>)
		;(T
		 <CCURSET ,BOTTOM-HINT-NUM 1>
		 <TELL "[CHR=" N .CHR "]">)>
	  <COND (<EQUAL? .CHR !\M !\m !\Q !\q ;,ESCAPE-KEY>
		 <SET Q T>
		 <RETURN>)
		(<EQUAL? .CHR ,LEFT-ARROW>
		 <SET NUM <- .QN ,BOTTOM-HINT-NUM>>
		 <COND (<L? 0 .NUM>
			<H-NEW-CURSOR .QN T>
			<H-NEW-CURSOR <SET QN .NUM>>
			<SETG H-QUEST-NUM 1>)
		       (T
			<SOUND ,S-BEEP>)>)
		(<EQUAL? .CHR ,RIGHT-ARROW>
		 <SET NUM <+ .QN ,BOTTOM-HINT-NUM>>
		 <COND (<G=? .MAXC .NUM>
			<H-NEW-CURSOR .QN T>
			<H-NEW-CURSOR <SET QN .NUM>>
			<SETG H-QUEST-NUM 1>)
		       (T
			<SOUND ,S-BEEP>)>)
		(<EQUAL? .CHR !\N !\n ,DOWN-ARROW>
		 <H-NEW-CURSOR <SET NUM .QN ;,H-CHAPT-NUM> T>
		 <REPEAT ()
			 <COND (<G? <SET NUM <+ .NUM 1>> .MAXC>
				<SET NUM 1>)>
			 <COND (<EQUAL? .NUM .QN ;,H-CHAPT-NUM>
				<SOUND ,S-BEEP>
				<H-NEW-CURSOR .QN ;,H-CHAPT-NUM>
				<RETURN>)
			       (<H-NEW-CURSOR .NUM>
				<SET QN ;H-CHAPT-NUM .NUM>
				<SETG H-QUEST-NUM 1>
				<RETURN>)>>)
		(<EQUAL? .CHR !\P !\p ,UP-ARROW>
		 <H-NEW-CURSOR <SET NUM .QN ;,H-CHAPT-NUM> T>
		 <REPEAT ()
			 <COND (<L? <SET NUM <- .NUM 1>> 1>
				<SET NUM .MAXC>)>
			 <COND (<EQUAL? .NUM .QN ;,H-CHAPT-NUM>
				<SOUND ,S-BEEP>
				<H-NEW-CURSOR .QN ;,H-CHAPT-NUM>
				<RETURN>)
			       (<H-NEW-CURSOR .NUM>
				<SET QN ;H-CHAPT-NUM .NUM>
				<SETG H-QUEST-NUM 1>
				<RETURN>)>>)
		(<EQUAL? .CHR 13 10 32>
		 <IFFLAG (SELECTIVE-HINTS
			  <SETG H-CHAPT-NUM <ZGET ,K-HINT-ITEMS .QN>>)
			 (T
			  <SETG H-CHAPT-NUM .QN>)>
		 <SET Q <H-PICK-QUESTION>>
		 <RETURN>)
		(<EQUAL? .CHR ,CLICK1 ,CLICK2>
		 <COND (<G? .TMP .MAXC>
			<SOUND ,S-BEEP>)
		       (T
			<H-NEW-CURSOR .QN ;,H-CHAPT-NUM T>
			<COND (<H-NEW-CURSOR .TMP>
			       <SET QN ;H-CHAPT-NUM .TMP>
			       <SETG H-QUEST-NUM 1>
			       <COND (<EQUAL? .CHR ,CLICK2>
				      <IFFLAG (SELECTIVE-HINTS
					       <SETG H-CHAPT-NUM
						     <ZGET ,K-HINT-ITEMS .QN>>)
					      (T
					       <SETG H-CHAPT-NUM .QN>)>
				      <SET Q <H-PICK-QUESTION>>
				      <RETURN>)>)
			      (T
			       <SOUND ,S-BEEP>
			       <H-NEW-CURSOR .QN ;,H-CHAPT-NUM>)>)>)
		(T
		 <SOUND ,S-BEEP>)>>
  <COND (<NOT .Q>
	 <AGAIN>)>>
  <CLEAR -1>
  <SCREEN ,S-WINDOW ;.WIN>
  <COND (<EQUAL? <LOWCORE INTID> ,APPLE-2C ,APPLE-2E ,APPLE-2GS>
	 <HLIGHT ,H-NORMAL>)
	(T
	 <COLOR <BAND .WCLR 255> <SHIFT .WCLR -8>>)>
  <SCREEN ,S-TEXT ;,S-FULL>
  <HLIGHT ,H-NORMAL>
  <WINATTR ,S-TEXT ,A-WRAP ,O-SET>
  <WINATTR ,S-TEXT ,A-SCRIPT ,O-SET>
  <CURSET -2 0> ;"turn cursor back on"
  <LEAVE-HINT-SCREEN>
  <RFATAL>>

<DEFINE H-PICK-QUESTION ("AUX" CHR MAXQ (Q <>) ;WIN WID QN)
  <SETG PARSE-SENTENCE-ACTIVATION <CATCH>>	;"for Q command"
  <HLIGHT ,H-NORMAL>
  <CLEAR ,S-TEXT>
  ;<SET WIN <INIT-HINT-SCREEN>>
  ;<WINATTR -3 ,A-SCRIPT ,O-CLEAR>
  <HINT-TITLE <GET <GET ,HINTS ,H-CHAPT-NUM> 1> ,S-WINDOW ;.WIN>
  <SET WID <CENTER-LINE ,M-MAIN-HINT-MENU 2 ;,H-INVERSE>>
  <SET MAXQ <- <GET <GET ,HINTS ,H-CHAPT-NUM> 0> 1>>
  <SCREEN ,S-TEXT ;,S-FULL>
  <SETG GET-HINT-ROUTINE ,H-GET-QUEST>
  <H-PUT-UP-FROBS .MAXQ ;<- <GET <GET ,HINTS ,H-CHAPT-NUM> 0> 1>>
  <IFFLAG (SELECTIVE-HINTS
	   <COND (<ZERO? <SET QN <INTBL? ,H-QUEST-NUM
				 <ZREST ,K-HINT-ITEMS 2>
				 <ZGET ,K-HINT-ITEMS 0>>>>
		  <SET QN 1>)
		 (T
		  <SET QN </ <- .QN ,K-HINT-ITEMS> 2>>)>)
	  (T
	   <SET QN ,H-QUEST-NUM>)>
  <COND (<G? .QN .MAXQ>
	 <PRINTI " [">
	 <PRINTN .QN>
	 <PRINTI "]">
	 <SET QN 1>)>
  <H-NEW-CURSOR .QN>
  <REPEAT (TMP NUM)
	  <COND (,DEMO-VERSION?
		 <SET CHR <INPUT-DEMO 1>>)
		(T
		 <SET CHR <INPUT 1>>)>
	  <COND (<EQUAL? .CHR ,CLICK1 ,CLICK2>
		 <SET TMP <SELECT-HINT-BY-MOUSE .WID T>>
		 <COND (<L=? .TMP 0>
			<COND (<EQUAL? .TMP -1>
			       <SET CHR !\N>)
			      (<EQUAL? .TMP -2>
			       <SET CHR !\P>)
			      (<EQUAL? .TMP -3>
			       <SET CHR 13>)
			      (<EQUAL? .TMP -4>
			       <SET CHR !\Q>)
			      (<EQUAL? .TMP -5>
			       <SET CHR !\M>)
			      (T
			       <SOUND ,S-BEEP>
			       <AGAIN>)>)>)
		;(T
		 <CCURSET ,BOTTOM-HINT-NUM 1>
		 <TELL "[CHR=" N .CHR "]">)>
	  <COND (<EQUAL? .CHR !\Q !\q ;,ESCAPE-KEY>
		 <RTRUE>)
		(<EQUAL? .CHR !\M !\m>
		 <SET Q T>
		 <RETURN>)
		(<EQUAL? .CHR ,LEFT-ARROW>
		 <SET NUM <- .QN ,BOTTOM-HINT-NUM>>
		 <COND (<L? 0 .NUM>
			<H-NEW-CURSOR .QN T>
			<H-NEW-CURSOR <SET QN .NUM>>)
		       (T
			<SOUND ,S-BEEP>)>)
		(<EQUAL? .CHR ,RIGHT-ARROW>
		 <SET NUM <+ .QN ,BOTTOM-HINT-NUM>>
		 <COND (<G=? .MAXQ .NUM>
			<H-NEW-CURSOR .QN T>
			<H-NEW-CURSOR <SET QN .NUM>>)
		       (T
			<SOUND ,S-BEEP>)>)
		(<EQUAL? .CHR !\N !\n ,DOWN-ARROW>
		 <H-NEW-CURSOR <SET NUM .QN> T>
		 <REPEAT ()
		         <IFFLAG (SELECTIVE-HINTS
				  <COND (<G? <SET NUM <+ .NUM 1>>
					     <ZGET ,K-HINT-ITEMS 0>>
					 <SET NUM 1>)>)
				 (T
				  <COND (<G? <SET NUM <+ .NUM 1>> .MAXQ>
					 <SET NUM 1>)>)>
			 <COND (<EQUAL? .NUM .QN>
				<SOUND ,S-BEEP>
				<RETURN>)
			       (<H-NEW-CURSOR .NUM>
				<SET QN .NUM>
				<RETURN>)>>
		 <H-NEW-CURSOR .QN>)
		(<EQUAL? .CHR !\P !\p ,UP-ARROW>
		 <H-NEW-CURSOR <SET NUM .QN> T>
		 <REPEAT ()
			 <COND (<L? <SET NUM <- .NUM 1>> 1>
				<IFFLAG (SELECTIVE-HINTS
					 <SET NUM <ZGET ,K-HINT-ITEMS 0>>)
					(T <SET NUM .MAXQ>)>)>
			 <COND (<EQUAL? .NUM .QN>
				<SOUND ,S-BEEP>
				<RETURN>)
			       (<H-NEW-CURSOR .NUM>
				<SET QN .NUM>
				<RETURN>)>>
		 <H-NEW-CURSOR .QN>)
		(<EQUAL? .CHR ,CLICK1 ,CLICK2>
		 <COND (<G? .TMP .MAXQ>
			<SOUND ,S-BEEP>)
		       (T
			<H-NEW-CURSOR .QN T>
			<COND (<H-NEW-CURSOR .TMP>
			       <SET QN .TMP>
			       <COND (<EQUAL? .CHR ,CLICK2>
				      <IFFLAG (SELECTIVE-HINTS
					       <SETG H-QUEST-NUM
						     <ZGET ,K-HINT-ITEMS .QN>>)
					      (T
					       <SETG H-QUEST-NUM .QN>)>
				      <DISPLAY-HINT>
				      <RETURN>)>)
			      (T
			       <SOUND ,S-BEEP>
			       <H-NEW-CURSOR .QN>)>)>)
		(<EQUAL? .CHR 13 10 32>
		 <IFFLAG (SELECTIVE-HINTS
			  <SETG H-QUEST-NUM <ZGET ,K-HINT-ITEMS .QN>>)
			 (T
			  <SETG H-QUEST-NUM .QN>)>
		 <DISPLAY-HINT>
		 <RETURN>)
		(T
		 <SOUND ,S-BEEP>)>>
  <COND (<NOT .Q>
	 <AGAIN>)>>

<DEFINE H-NEW-CURSOR (POS "OPT" (OFF? <>) "AUX" Y X)
	<SET Y .POS>
	<SET X ,LEFT-HINT-COLUMN>
	<COND (<G? .POS ,BOTTOM-HINT-NUM>
	       <SET Y <- .Y ,BOTTOM-HINT-NUM>>
	       <SET X </ <WINGET -3 ,WWIDE> ;<LOWCORE SCRH> 2>>)>
	<CURSET <+ 1 <* <- .Y 1> <SHIFT <WINGET -3 ,WFSIZE> -8>>> .X>
	<COND (<NOT .OFF?>
	       <HLIGHT ,H-INVERSE>)
	      (T
	       <HLIGHT ,H-NORMAL>)>
	<COND (<SET X <ZAPPLY ,GET-HINT-ROUTINE
			      <IFFLAG (SELECTIVE-HINTS
				       <ZGET ,K-HINT-ITEMS .POS>)
				      (T
				       .POS)>>>
	       <TELL .X !\ >)>
	<COND (<NOT .OFF?>
	       <HLIGHT ,H-NORMAL>)>
	.X>

<DEFINE SELECT-HINT-BY-MOUSE ("OPT" (WID 0) (Q? <>) "AUX" VAL MID X Y WIN)
	<SET Y <LOWCORE MSLOCY>>
	<SET X <LOWCORE MSLOCX>>
	<SET VAL <WINGET ,S-WINDOW ,WTOP>>
	<COND (<AND <L=? .VAL .Y>
		    <L? <- .Y .VAL> <WINGET ,S-WINDOW ,WHIGH>>>
	       <SET WIN ,S-WINDOW>)
	      (T
	       <SET VAL <WINGET ,S-TEXT ,WTOP>>
	       <SET WIN ,S-TEXT>)>
	;<CCURSET ,BOTTOM-HINT-NUM 1>
	;<TELL "[">
	<SET Y <- .Y .VAL>>
	<SET VAL <SHIFT <WINGET .WIN ;-3 ,WFSIZE> -8>>	;"font height"
	<SET VAL </ .Y ;<- .Y 1> .VAL>>	;"line number"
	<SET MID </ <* <LOWCORE SCRH> <BAND <WINGET .WIN ;-3 ,WFSIZE> *377*>>
		    ;<WINGET .WIN ;-3 ,WWIDE>
		    2>>
	;<TELL "LN=" N .VAL " X=" N .X " MID=" N .MID " TWID=" N .WID>
	<COND (<EQUAL? .WIN ,S-WINDOW>
	       <COND (<EQUAL? .VAL 1>
		      <COND (<AND <SET VAL </ .WID 2>>
				  <G? .X <- .MID .VAL>>
				  <L? .X <+ .MID .VAL>>>
			     ;<TELL " VAL=-5">
			     <RETURN -5>)
			    (<L=? .X .MID>
			     ;<TELL " VAL=-1">
			     <RETURN -1>)
			    (T
			     ;<TELL " VAL=-3">
			     <RETURN -3>)>)
		     (<EQUAL? .VAL 2>
		      <SET VAL </ ,H-OR-USE-MOUSE-WID 2>>
		      <COND (<L? .X <- .MID .VAL>>
			     ;<TELL " VAL=-2">
			     <RETURN -2>)
			    (<G? .X <+ .MID .VAL>>
			     ;<TELL " VAL=-4">
			     <RETURN -4>)>)
		     (T
		      <COND (T
			     ;<TELL " VAL=0">
			     <RETURN 0>)>)>)
	      (T
	       <SET VAL <+ 1 .VAL>>
	       ;<TELL " VAL=" N .VAL>
	       <COND (<AND <G? .X .MID> <NOT .Q?>>
		      <SET VAL <+ .VAL ,BOTTOM-HINT-NUM>>
		      ;<TELL " -> " N .VAL>)
		     (<G? .VAL ,BOTTOM-HINT-NUM>
		      <SET VAL ,BOTTOM-HINT-NUM>
		      ;<TELL " -> " N .VAL>)>
	       ;<TELL "]|">
	       .VAL)>>

<DEFINE INVERSE-LINE ("OPT" (LN 0) (INV ,H-INVERSE))
	<COND (<T? .LN>
	       <CCURSET .LN 1>)>
	<HLIGHT .INV>
	<COND (<EQUAL? .INV ,H-NORMAL>
	       <ERASE 1>)
	      (T
	       <FONT 4>
	       <SET LN <LOWCORE (FWRD 1)>>
	       <SET INV <WINGET -3 ,WWIDE>>
	       <PRINT-SPACES </ <+ .LN .INV> .LN>>
	       <COND (<EQUAL? <LOWCORE INTID> ,APPLE-2E ,APPLE-2C ,APPLE-2GS>
		      <IFFLAG (SELECTIVE-HINTS
			       <CURGET ,K-DIROUT-TBL>
			       <CURSET <GET ,K-DIROUT-TBL 0> <- .INV .LN>>)
			      (T
			       <CURGET ,SLINE>
			       <CURSET <GET ,SLINE 0> <- .INV .LN>>)>
		      <PRINTI " ">)>
	       <FONT 1>
	       <HLIGHT ,H-NORMAL>)>>

<IF-SELECTIVE-HINTS
<DEFINE PRINT-SPACES (CNT)
	 <REPEAT ()
		 <COND (<L? <SET CNT <- .CNT 1>> 0>
			<RETURN>)
		       (T
			<PRINTC 32>)>>>

<GLOBAL DEMO-VERSION? <>>
<CONSTANT SLIDE-SHOW-TIMEOUT 150>
<CONSTANT DEMO-TIMEOUT 600>

<ROUTINE INPUT-DEMO (ARG "AUX" CHR)
	<SETG DEMO-VERSION? -1>
	<SET CHR <INPUT .ARG ,DEMO-TIMEOUT ,SLIDE-SHOW-HANDLER>>
	<COND (<1? ,DEMO-VERSION?>
	       <END-DEMO>)
	      (T
	       <WINPUT ,S-TEXT 15 -999>	;"Disable MORE counter."
	       .CHR)>>

<ROUTINE END-DEMO ()
	<CLEAR -1>
	<TELL "|
You have reached the end of this demonstration version of|">
	<V-VERSION>
	<TELL "|
|
Hit any key to start over...">
	<INPUT 1 ,SLIDE-SHOW-TIMEOUT ,SLIDE-SHOW-HANDLER>
	<SCREEN ,S-TEXT>
	<COLOR 1 1> ;"return to default before screen clears"
	<RESTART>
	;<TELL ,FAILED>
	<AGAIN>>

<ROUTINE SLIDE-SHOW-HANDLER ()
	<SETG DEMO-VERSION? +1>
	<RTRUE>>>

<DEFINE DISPLAY-HINT ("AUX" H MX CNT CV SHIFT? COUNT-OFFS ;WIN WID)
  <HLIGHT ,H-NORMAL>
  <CLEAR ,S-TEXT>
  ;<SET WIN <INIT-HINT-SCREEN>>
  <SCREEN ,S-WINDOW ;.WIN>
  <INVERSE-LINE 3 ,H-NORMAL>
  <RIGHT-LINE ,Q-RESUME-STORY 3 ;,H-INVERSE>
  <INVERSE-LINE 2 ,H-NORMAL>
  <RIGHT-LINE ,RETURN-SEE-NEW-HINT 2 ;,H-INVERSE>
  <COND (<NOT <EQUAL? <BAND <LOWCORE FLAGS> 32> 0>>
	 <SETG H-OR-USE-MOUSE-WID <CENTER-LINE ,H-OR-USE-MOUSE 3 ;,H-INVERSE>>)>
  <INVERSE-LINE 1 ,H-NORMAL>
  <HLIGHT ,H-BOLD>
  <SET H <GET <GET ,HINTS ,H-CHAPT-NUM> <+ ,H-QUEST-NUM 1>>>
  ;"Byte table to use for showing questions already seen.
    Actually a nibble table.  The high four bits of each byte are for
    H-QUEST-NUM odd; the low four bits are for H-QUEST-NUM even.  See SHIFT?
    and COUNT-OFFS."
  <SET CV <GET ,HINT-COUNTS <- ,H-CHAPT-NUM 1>>>
  <CENTER-LINE <GET .H <IFFLAG (SELECTIVE-HINTS 2) (T 1)>> 1 ,H-INVERSE>
  <HLIGHT ,H-NORMAL>
  <SET WID <CENTER-LINE ,M-SEE-HINT-MENU 2 ;,H-INVERSE>>
  <SET MX <GET .H 0>>
  <DIROUT ,D-SCREEN-OFF>	;"Put question in transcript."
  <PRINT <GET .H <IFFLAG (SELECTIVE-HINTS 2) (T 1)>>>
  <CRLF>
  <DIROUT ,D-SCREEN-ON>
  <SCREEN ,S-TEXT>
  <WINATTR -3 ,A-SCRIPT ,O-SET>
  <WINATTR ,S-TEXT ,A-WRAP ,O-SET>
  <CURSET 1 1>
  <SET SHIFT? <MOD ,H-QUEST-NUM 2>>
  <SET COUNT-OFFS </ <- ,H-QUEST-NUM 1> 2>>
  <SET CNT <IFFLAG (SELECTIVE-HINTS 2) (T 1)>>
  <REPEAT ((CURCX <GETB .CV .COUNT-OFFS>)
	   (CURC <+ 1 <ANDB <COND (.SHIFT? <LSH .CURCX -4>)
				  (T .CURCX)> *17*>>))
	  <COND (<G? <SET CNT <+ .CNT 1>> .CURC>
		 <RETURN>)
		(T
		 <TELL N <+ <- .MX .CNT> 1> "> " <GET .H .CNT> CR>)>>
  <REPEAT (CHR ;N TMP (FLG T))
     <COND (.FLG
	    <SET FLG <>>
	    <COND (<G? .CNT .MX>
		   <PRINT ,NO-MORE-HINTS>
		   <CRLF>
		   <SCREEN ,S-WINDOW ;.WIN>
		   <INVERSE-LINE 2 ,H-NORMAL>
		   <CENTER-LINE ,M-SEE-HINT-MENU 2 ;,H-INVERSE>
		   <SCREEN ,S-TEXT>)
		  (T
		   <TELL ;"[" N <+ <- .MX .CNT> 1> ;" hint">
		   ;<COND (<NOT <EQUAL? .N 1>>
			  <TELL "s">)>
		   <TELL ;" left.] -" "> ">)>)>
     <COND (,DEMO-VERSION?
	    <SET CHR <INPUT-DEMO 1>>)
	   (T
	    <SET CHR <INPUT 1>>)>
     <COND (<EQUAL? .CHR ,CLICK1 ,CLICK2>
	    <SET TMP <SELECT-HINT-BY-MOUSE .WID>>
	    <COND (<L=? .TMP 0>
		   <COND (<EQUAL? .TMP -3>
			  <SET CHR 13>)
			 (<EQUAL? .TMP -4>
			  <SET CHR !\Q>)
			 (<EQUAL? .TMP -5>
			  <SET CHR !\M>)
			 (T
			  <SOUND ,S-BEEP>
			  <AGAIN>)>)>)
	   ;(T
	    <CCURSET ,BOTTOM-HINT-NUM 1>
	    <TELL "[CHR=" N .CHR "]">)>
     <COND (<EQUAL? .CHR !\M !\m !\Q !\q ;,ESCAPE-KEY>
	    <COND (.SHIFT?
		   <PUTB .CV .COUNT-OFFS
			 <ORB <ANDB <GETB .CV .COUNT-OFFS> *17*>
			      <LSH <- .CNT 2> 4>>>)
		  (T
		   <PUTB .CV .COUNT-OFFS
			 <ORB <ANDB <GETB .CV .COUNT-OFFS> *360*>
			      <- .CNT 2>>>)>
	    <COND (<EQUAL? .CHR !\Q !\q ;,ESCAPE-KEY>
		   <THROW T ,PARSE-SENTENCE-ACTIVATION>)
		  (T
		   <WINATTR -3 ,A-SCRIPT ,O-CLEAR>
		   <WINATTR ,S-TEXT ,A-WRAP ,O-CLEAR>
		   <RETURN>)>)
	   (<EQUAL? .CHR 13 10 ;"32 ,CLICK1 ,CLICK2">
	    <COND (<L=? .CNT .MX>
		   <SET FLG T>	;"CNT starts as 2/3 or more"
		   <TELL <GET .H .CNT> CR>
		   <COND (<G? <SET CNT <+ .CNT 1>> .MX>
			  <SET FLG <>>
			  <PRINT ,NO-MORE-HINTS>
			  <CRLF>
			  <SCREEN ,S-WINDOW ;.WIN>
			  <INVERSE-LINE 2 ,H-NORMAL>
			  <CENTER-LINE ,M-SEE-HINT-MENU 2 ;,H-INVERSE>
			  <SCREEN ,S-TEXT>)>)
		  (T
		   <SOUND ,S-BEEP>)>)
	   (T
	    <SOUND ,S-BEEP>)>>>

<IF-SELECTIVE-HINTS
<CONSTANT S-FULL 7>
<CONSTANT K-HINT-ITEMS <ITABLE 38 0>>
<DEFINE RT-SEE-QST? (OBJ)
	<COND (<NOT .OBJ>
	       T)
	      (<NOT <OBJECT? .OBJ>>
	       <ZAPPLY .OBJ>)
	      (<IN? .OBJ ,ROOMS>
	       <FSET? .OBJ ,TOUCHBIT>)
	      (T
	       <FSET? .OBJ ,SEENBIT>)>>>

<DEFINE H-CHAPT-NAME ACT (N)
 <IFFLAG (SELECTIVE-HINTS
	  <REPEAT ((I 0) (MAX <- <GET <GET ,HINTS .N> 0> 1>))
		<COND (<IGRTR? .I .MAX>
		       <RETURN <> .ACT>)
		      (T
		       <COND (<RT-SEE-QST?
				 <GET <GET <GET ,HINTS .N> <+ .I 1>> 1>>
			      <RETURN <GET <GET ,HINTS .N> 1> .ACT>)>)>>)
	 (T
	  <GET <GET ,HINTS .N> 1>)>>

<DEFINE H-GET-QUEST (N)
 <IFFLAG (SELECTIVE-HINTS
	  <COND (<RT-SEE-QST? <GET <GET <GET ,HINTS ,H-CHAPT-NUM> <+ .N 1>> 1>>
		 <RETURN <GET <GET <GET ,HINTS ,H-CHAPT-NUM> <+ .N 1>> 2>>)
		(T
		 <RETURN <>>)>)
	 (T
	  <GET <GET <GET ,HINTS ,H-CHAPT-NUM> <+ .N 1>> 1>)>>

<DEFINE H-PUT-UP-FROBS (MX)
  <HLIGHT ,H-NORMAL>
  <REPEAT ((ST 0) (QN 1) (X ,LEFT-HINT-COLUMN) (Y ,TOP-HINT-LINE)
	   TMP (BHL ,BOTTOM-HINT-NUM)
	   (WID <SHIFT <WINGET -3 ,WFSIZE> -8>))
	<COND (<G? <SET ST <+ .ST 1>> .MX>
	       <IF-SELECTIVE-HINTS
			<ZPUT ,K-HINT-ITEMS 0 <- .QN 1>>>
	       <RETURN>)>
	<COND (<SET TMP <ZAPPLY ,GET-HINT-ROUTINE .ST>>
	       <CURSET <+ 1 <* <- .Y 1> .WID>> .X>
	       <TELL .TMP>
	       <ERASE 1> ;"to erase disk-flipping msg"
	       <IF-SELECTIVE-HINTS
		<ZPUT ,K-HINT-ITEMS .QN .ST>
		<INC QN>>
	       <COND (<G? <SET Y <+ 1 .Y>> .BHL>
		      <SET Y ,TOP-HINT-LINE>
		      <SET X </ <WINGET -3 ,WWIDE> ;<LOWCORE SCRH> 2>>)>)>>>

<DEFAULT-DEFINITION INIT-HINT-SCREEN
<DEFINE INIT-HINT-SCREEN ()
  <CLEAR -1>
  <CSPLIT 4>
  <SCREEN ,S-TEXT>
  ,S-WINDOW>>

<DEFAULT-DEFINITION LEAVE-HINT-SCREEN
<DEFINE LEAVE-HINT-SCREEN ()
  <INIT-STATUS-LINE>
  <IF-SOUND <COND (,SOUND-ON?
		   <CHECK-LOOPING>)>>
  <TELL "Back to the story..." CR>>>

<DEFINE HINT-TITLE (TITLE WIN "OPTIONAL" (THIRD T))
  <SCREEN ,S-WINDOW ;.WIN>
  <INVERSE-LINE 1 ,H-NORMAL>
  <INVERSE-LINE 2 ,H-NORMAL>
  <INVERSE-LINE 3 ,H-NORMAL>
  <HLIGHT ,H-BOLD>
  <CENTER-LINE .TITLE 1 ,H-INVERSE>
  <HLIGHT ,H-NORMAL>
  <LEFT-LINE 2 ,NEXT-HINT ;,H-INVERSE>
  <COND (<NOT <EQUAL? <BAND <LOWCORE FLAGS> 32> 0>>
	 <SETG H-OR-USE-MOUSE-WID <CENTER-LINE ,H-OR-USE-MOUSE 3 ;,H-INVERSE>>)>
  <LEFT-LINE 3 ,PREVIOUS-HINT ;,H-INVERSE>
  <COND (.THIRD
	 <RIGHT-LINE ,RETURN-SEE-HINT 2 ;,H-INVERSE>
	 <RIGHT-LINE ,Q-RESUME-STORY 3 ;,H-INVERSE>)>>

<DEFINE LEFT-LINE (LN STR "OPTIONAL" (INV <>))
	<CCURSET .LN 1>
	<COND (.INV
	       <HLIGHT .INV>)>
	<TELL .STR>
	<COND (.INV
	       <HLIGHT ,H-NORMAL>)>>

<DEFINE RIGHT-LINE (STR "OPTIONAL" (LN 0) (INV <>) (LEN 0))
	<JUSTIFIED-LINE .STR .LN .INV .LEN 1>>

<DEFINE CENTER-LINE (STR "OPTIONAL" (LN 0) (INV <>) (LEN 0))
	<JUSTIFIED-LINE .STR .LN .INV .LEN 2>>

<DEFINE JUSTIFIED-LINE (STR LN INV LEN CTR)
  <COND (<ZERO? .LN>
	 <IFFLAG (SELECTIVE-HINTS
		  <CURGET ,K-DIROUT-TBL>
		  <SET LN <GET ,K-DIROUT-TBL 0>>)
		 (T
		  <CURGET ,SLINE>
		  <SET LN <GET ,SLINE 0>>)>)
	(T
	 <SET LN <- .LN 1>>
	 <SET LN <+ 1 <* .LN <SHIFT <WINGET -3 ,WFSIZE> -8>>>>)>
  <COND (<ZERO? .LEN>
	 <IFFLAG (SELECTIVE-HINTS
		  <DIROUT ,D-TABLE-ON ,K-DIROUT-TBL>)
		 (T
		  <DIROUT ,D-TABLE-ON ,SLINE ;-80>)>
	 <TELL .STR !\ >
	 <DIROUT ,D-TABLE-OFF>
	 <SET LEN <LOWCORE TWID>>)>
  <CURSET .LN </ <- <WINGET -3 ,WWIDE> .LEN> .CTR>>
  <COND (.INV
	 <HLIGHT .INV>)>
  <TELL .STR !\ >
  <COND (.INV
	 <HLIGHT ,H-NORMAL>)>
  .LEN>

<END-SEGMENT> ;<IFN-SELECTIVE-HINTS <END-SEGMENT>>
