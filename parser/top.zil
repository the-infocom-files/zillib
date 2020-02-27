"TOP for NEW PARSER
Copyright (C) 1988 Infocom, Inc.  All rights reserved."

<INCLUDE "basedefs" "pbitdefs" "pdefs">

<FILE-FLAGS MDL-ZIL?>

<BEGIN-SEGMENT 0>

<DEFAULTS-DEFINED
	ADJ-USED?
	ASKING-VERB-WORD?
	CANT-UNDO
	CAPITAL-NOUN?
	;DIR-VERB-PRSI?
	;DIR-VERB-WORD?
	FIND-A-WINNER
	;I-ASSUME-STRING
	ITAKE-CHECK
	META-LOC
	MORE-SPECIFIC
	NO-M-WINNER-VERB?
	NOT-HERE
	;NOT-HERE-VERB?
	NOUN-USED?
	OWNERS
	P-PRONOUNS
	SEE-VERB?
	SIBREAKS
	SPEAKING-VERB?
	;TELL-I-ASSUME
	TELL-PRONOUN
	TELL-SAID-TO
	TELL-TOO-DARK
	VERB-ALL-TEST>

<DEFAULT-DEFINITION SIBREAKS
	<SETG20 SIBREAKS ".,\"'!?">>
<DEFAULT-DEFINITION OWNERS
	<CONSTANT OWNERS <TABLE (PURE LENGTH) PLAYER>>>

;<DEFAULT-DEFINITION I-ASSUME-STRING
	<CONSTANT I-ASSUME "[I assume you mean:">>

;<DEFAULT-DEFINITION TELL-I-ASSUME
 <DEFINE TELL-I-ASSUME (OBJ "OPT" PRON)
	<COND (<AND <NOT <FSET? .PRON ,TOUCHBIT>> 
		    <NOT <EQUAL? ,OPRSO .OBJ>>>
	       <FSET .PRON ,TOUCHBIT>
	       <TELL ,I-ASSUME>
	       <TELL !\ >
	       <TELL-THE .OBJ>
	       <TELL ".]" CR>)>>>

<DEFAULT-DEFINITION MORE-SPECIFIC
 <DEFINE MORE-SPECIFIC ()
	<SETG CLOCK-WAIT T>
	<TELL "[Please be more specific.]" CR>>>

<IF-P-DEBUGGING-PARSER
<SYNTAX \#DBG = V-PDEBUG>

<GLOBAL P-DBUG:FLAG <>>
<GLOBAL IDEBUG:FLAG <>>
<CONSTANT G-DEBUG 69>

<DEFINE V-PDEBUG ()
 <COND (<T? ,PRSO>
	<SETG IDEBUG <NOT ,IDEBUG>>
	<TELL !\{ N ,IDEBUG "}" CR>)
       (<SETG P-DBUG <NOT ,P-DBUG>>
	<TELL "Find them bugs, boss!" CR>)
       (T <TELL "No bugs left, eh?" CR>)>>>

<SETG P-PRSI <>>
<SETG P-PRSO <>>
<SETG PRSA 0 <OR VERB FALSE>>
<IF-P-BE-VERB
	<GLOBAL PRSQ 0>
	<GLOBAL PRSS:OBJECT 0>>
<GLOBAL PRSI:OBJECT 0>
<GLOBAL PRSO:OBJECT 0>

<GLOBAL P-MULT <>>
<GLOBAL OPRSO <>>
<GLOBAL P-CONT:NUMBER 0>

<CONSTANT P-LEXWORDS 1>	"Byte offset to # of entries in LEXV"
<CONSTANT P-LEXSTART 1>	"Word offset to start of LEXV entries"
<CONSTANT P-LEXELEN 2>	"Number of words per LEXV entry"
<CONSTANT P-WORDLEN 4>

<GLOBAL P-WON <>>

"<CONSTANT M-FATAL 2>"
<DEFMAC RFATAL ()
	'<PROG () <PUSH 2> <RSTACK>>>   

"<CONSTANT M-BEG 1>  
<CONSTANT M-END 6> 
<CONSTANT M-CONT 7> 
<CONSTANT M-WINNER 8>"

<GLOBAL P-PRSA-WORD <>>

<GLOBAL PRSO-NP <>>
<GLOBAL PRSI-NP <>>
<GLOBAL CLOCKER-RUNNING:NUMBER 2>

<IF-UNDO <GLOBAL P-CAN-UNDO:NUMBER 0>>

<DEFAULT-DEFINITION VERB-ALL-TEST
<DEFINE VERB-ALL-TEST (O I "AUX" L)	;"O=PRSO I=PRSI"
 <SET L <LOC .O>>
 <COND (<VERB? DROP GIVE>
	<COND (<EQUAL? .L ,WINNER>
	       <RTRUE>)
	      (T <RFALSE>)>)
       (<VERB? PUT>
	<COND (<EQUAL? .O .I>
	       <RFALSE>)
	      (<NOT <IN? .O .I>>
	       <RTRUE>)
	      (T <RFALSE>)>)
       (<VERB? TAKE>
	<COND (<AND <NOT <FSET? .O ,TAKEBIT>>
		    <NOT <FSET? .O ,TRYTAKEBIT>>>
	       <RFALSE>)>
	<COND (<NOT <ZERO? .I>>
	       <COND (<NOT <EQUAL? .L .I>>
		      <RFALSE>)>)
	      (<EQUAL? .L ;,WINNER ,HERE>
	       <RTRUE>)>
	<COND (<OR <FSET? .L ,PERSONBIT>
		   <FSET? .L ,SURFACEBIT>>
	       <RTRUE>)
	      (<AND <FSET? .L ,CONTBIT>
		    <FSET? .L ,OPENBIT>>
	       <RTRUE>)
	      (T <RFALSE>)>)
       (<NOT <ZERO? .I>>
	<COND (<NOT <EQUAL? .O .I>>
	       <RTRUE>)
	      (T <RFALSE>)>)
       (T <RTRUE>)>>>

<DEFINE FIX-HIM-HER-IT (PRON OBJ)
 <COND (<ZERO? .OBJ>
	<MORE-SPECIFIC>
	<>)
       (<AND <NOT <ACCESSIBLE? ;VISIBLE? .OBJ>>
	     <OR <AND <EQUAL? .PRON ,PRSO>
		      <NOT <EVERYWHERE-VERB? 1>>>
		 <AND <EQUAL? .PRON ,PRSI>
		      <NOT <EVERYWHERE-VERB? 2>>>>>
	<NOT-HERE .OBJ>
	<>)
       (T
	<COND (<EQUAL? ,PRSO .PRON>
	       <SETG PRSO .OBJ>
	       <TELL-PRONOUN .OBJ .PRON>)>
	<COND (<EQUAL? ,PRSI .PRON>
	       <SETG PRSI .OBJ>
	       <TELL-PRONOUN .OBJ .PRON>)>
	<IF-P-BE-VERB
	<COND (<EQUAL? ,PRSS .PRON>
	       <SETG PRSS .OBJ>
	       <TELL-PRONOUN .OBJ .PRON>)>>
	T)>>

<DEFAULT-DEFINITION TELL-PRONOUN
<DEFINE TELL-PRONOUN (OBJ PRON)
 <COND (<AND <NOT <FSET? .PRON ,TOUCHBIT>> 
	     <NOT <EQUAL? ,OPRSO .OBJ>>>
	<IF-P-BE-VERB <COND (<EQUAL? ,PRSA ,V?DO?> <RFALSE>)>>
	<TELL "[\"">
	<TELL D ;PRINTB .PRON>
	<TELL "\" meaning ">
	<TELL-THE .OBJ>
	<TELL "]" CR>)>>>

<DEFAULT-DEFINITION NO-M-WINNER-VERB?
<CONSTANT NO-M-WINNER-VERB-TABLE
 <PLTABLE V?TELL-ABOUT V?SGIVE V?SSHOW V?SRUB V?SPUT-ON>>

<DEFINE NO-M-WINNER-VERB? ()
 <COND (<INTBL? ,PRSA <ZREST ,NO-M-WINNER-VERB-TABLE 2>
		       <ZGET ,NO-M-WINNER-VERB-TABLE 0>>
	<RTRUE>)>>>

<DEFAULT-DEFINITION FIND-A-WINNER
<DEFINE FIND-A-WINNER ACT ("OPT" (RM ,HERE))
 <COND (<AND <T? ,QCONTEXT>
	     <IN? ,QCONTEXT .RM>>
	,QCONTEXT)
       (T
	<REPEAT ((OTHER <FIRST? .RM>) (WHO <>) (N 0))
		<COND (<ZERO? .OTHER>
		       <RETURN .WHO .ACT>)
		      (<AND <FSET? .OTHER ,PERSONBIT>
			    <NOT <FSET? .OTHER ,INVISIBLE>>
			    <NOT <EQUAL? .OTHER ,PLAYER>>>
		       <COND (<G? <SET N <+ 1 .N>> 1>
			      <RETURN <> .ACT>)>
		       <SET WHO .OTHER>)>
		<SET OTHER <NEXT? .OTHER>>>)>>>

<DEFAULT-DEFINITION TELL-SAID-TO
 <DEFINE TELL-SAID-TO (PER) <TELL "[said to " D .PER "]" CR>>>

<GLOBAL QCONTEXT:OBJECT <>>

<DEFINE QCONTEXT-GOOD? ()
 <COND (<AND <NOT <ZERO? ,QCONTEXT>>
	     <FSET? ,QCONTEXT ,PERSONBIT>
	     ;<NOT <FSET? ,QCONTEXT ,MUNGBIT>>
	     <EQUAL? ,HERE <META-LOC ,QCONTEXT>>>
	<RETURN ,QCONTEXT>)>>

<DEFAULT-DEFINITION META-LOC
<DEFINE META-LOC ML (OBJ "OPTIONAL" (INV <>) "AUX" L)
	<SET L <LOC .OBJ>>
	<REPEAT ()
		<COND (<EQUAL? <> .OBJ .L>
		       <RETURN <> .ML>)
		      (<EQUAL? .L
			       ,LOCAL-GLOBALS ,GLOBAL-OBJECTS ,GENERIC-OBJECTS>
		       <RETURN .L .ML>)
		      (<IN? .OBJ ,ROOMS>
		       <RETURN .OBJ .ML>)
		      (T
		       <COND (<AND .INV <FSET? .OBJ ,INVISIBLE>>
			      <RETURN <> .ML>)>
		       <SET OBJ .L>
		       <SET L <LOC .OBJ>>)>>>>

<DEFAULT-DEFINITION P-PRONOUNS
<GLOBAL P-IT-OBJECT:OBJECT <>>
<GLOBAL P-THEM-OBJECT:OBJECT <>>
<GLOBAL P-HER-OBJECT:OBJECT <>>
<GLOBAL P-HIM-OBJECT:OBJECT <>>>

;<DEFINE NOT-IT (WHO)
 <COND (<EQUAL? .WHO ,P-HER-OBJECT>
	<FCLEAR ,HER ,TOUCHBIT>)
       (<EQUAL? .WHO ,P-HIM-OBJECT>
	<FCLEAR ,HIM ,TOUCHBIT>)
       (<EQUAL? .WHO ,P-THEM-OBJECT>
	<FCLEAR ,THEM ,TOUCHBIT>)
       (<EQUAL? .WHO ,P-IT-OBJECT>
	<FCLEAR ,IT  ,TOUCHBIT>)>>

<DEFAULT-DEFINITION CANT-UNDO
<IF-UNDO
<DEFINE CANT-UNDO ()
	<TELL "[I can't undo that now.]" CR>>>>

<GLOBAL NOW-PRSI:FLAG <>>

<GLOBAL OBJ-SWAP:FLAG <>>

<OBJECT NOT-HERE-OBJECT
	(CONTFCN 0)
	(THINGS 0)>

<DEFAULT-DEFINITION SEE-VERB?
	<DEFINE SEE-VERB? ()
	<VERB? CHASTISE EXAMINE FIND
	       LOOK LOOK-BEHIND LOOK-DOWN LOOK-INSIDE LOOK-UNDER LOOK-UP
	       READ SEARCH>>>

<DEFINE PERFORM (PA "OPT" (PO <>) (PI <>) ;(PQ <>) ;(PS <>)
		    "AUX" V OA OO OI OQ OS X)
	<SET OA ,PRSA>
	<SET OO ,PRSO>
	<SET OI ,PRSI>
	<COND (<AND <T? .OO> <==? .OO .PI>>
	       <SETG OBJ-SWAP T>)
	      (<AND <T? .OI> <==? .OI .PO>>
	       <SETG OBJ-SWAP T>)
	      (T
	       <SETG OBJ-SWAP <>>)>
	<SETG PRSA .PA>
	<SETG PRSI .PI>
	<SETG PRSO .PO>
	;<IF-P-BE-VERB
	 <SET OS ,PRSS>
	 <SET OQ ,PRSQ>
	 <SETG PRSS .PS>
	 <SETG PRSQ .PQ>>
	<IF-P-DEBUGGING-PARSER
	 <COND (<T? ,P-DBUG>
	       <PRINTI "{Perform: A=">
	       <IFFLAG (IN-ZILCH <PRINTN .PA>)
		       (T <PRINC <NTH ,ACTIONS <+ <* .PA 2> 1>>>)>
	       <COND (<T? .PO>
		      <PRINTI "/O=">
		      <COND (<DIR-VERB?> <PRINTN .PO>)
			    (T <TELL-D-LOC .PO>)>)>
	       <COND (<T? .PI>
		      <PRINTI "/I=">
		      <TELL-D-LOC .PI>)>
	       <IF-P-BE-VERB
	       <COND (<T? ,PRSQ ;.PQ>
		      <PRINTI "/Q=">
		      <IFFLAG (IN-ZILCH <PRINTN ,PRSQ ;.PQ>)
			      (T <PRINC <NTH ,ACTIONS <+ <* ,PRSQ ;.PQ 2> 1>>>)>)>
	       <COND (<T? ,PRSS ;.PS>
		      <PRINTI "/S=">
		      <TELL-D-LOC ,PRSS ;.PS>)>>
	       <PRINTI "}|">)>>
	<SET V <>>
	<IF-P-BE-VERB
	 <COND (<T? ,PRSS>
		<THIS-IS-IT ,PRSS>)>>
	<COND (<T? ,PRSI>
	       <THIS-IS-IT ,PRSI>)>
	<COND (<AND <T? ,PRSO>
		    <NOT <VERB? TELL>>	;"per PDL 14-Dec-88"
		    <NOT <DIR-VERB?>>>
	       <THIS-IS-IT ,PRSO>)>
	<COND (<NOT <EQUAL? ,WINNER ,PLAYER>>
	       <THIS-IS-IT ,WINNER>)>
	<SET PO ,PRSO>
	<SET PI ,PRSI>
	;<IF-P-BE-VERB <SET PS ,PRSS>>
	<COND (<AND ;<ZERO? .V>
		    <NOT <NO-M-WINNER-VERB?>>>
	       <IFFLAG (P-DEBUGGING-PARSER
			<SET V <D-APPLY "Winner" <GETP ,WINNER ,P?ACTION>
					,M-WINNER>>)
		       (T
			<SET V <ZAPPLY <GETP ,WINNER ,P?ACTION> ,M-WINNER>>)>)>
<IFFLAG (P-BE-VERB
	<COND (<T? ,PRSS ;.PS>
	       <COND (<ZERO? .V>
		      <IFFLAG (P-DEBUGGING-PARSER
			       <SET V <D-APPLY "Subject"
					       <GETP ,PRSS ;.PS ,P?ACTION>
					       ,M-SUBJ>>)
			      (T
			       <SET V <ZAPPLY <GETP ,PRSS ,P?ACTION>
					      ,M-SUBJ>>)>)>
	       <COND (<AND <ZERO? .V>
			   <T? ,PRSQ ;.PQ>>
		      <COND (<SET X <INTBL? <ZGET ,ACTIONS .PA>
					    <ZREST ,QACTIONS 2>
					    <ZGET ,QACTIONS 0>>>
			     <IFFLAG (P-DEBUGGING-PARSER
				      <SET V <D-APPLY "Preaction"<ZGET .X 2>>>)
				     (T
				      <SET V <ZAPPLY <ZGET .X 2>>>)>)>)>
	       <COND (<ZERO? .V>
		      <COND (<T? ,PRSQ ;.PQ>
			     <COND (<SET X <INTBL? <ZGET ,ACTIONS .PA>
						   <ZREST ,QACTIONS 2>
						   <ZGET ,QACTIONS 0>>>
				    <COND (<SET X <ZGET .X 1>>
					   <IFFLAG (P-DEBUGGING-PARSER
						    <SET V <D-APPLY <> .X>>)
						   (T
						    <SET V <ZAPPLY .X>>)>)>)>
			     <COND (<ZERO? .V>
				    <IFFLAG (P-DEBUGGING-PARSER
					<SET V <D-APPLY <>
						       <ZGET ,ACTIONS ,PRSQ>>>)
					    (T
					<SET V
					   <ZAPPLY <ZGET ,ACTIONS ,PRSQ>>>)>)>)
			    (T
			     <IFFLAG (P-DEBUGGING-PARSER
				      <D-APPLY <> ,V-STATEMENT>)
				     (T
				      <ZAPPLY ,V-STATEMENT>)>)>)>
	       <COND (<EQUAL? ,M-FATAL .V>
		      <SETG P-CONT -1>)>
	       <SETG PRSA .OA>
	       <SETG PRSO .OO>
	       <SETG PRSI .OI>
	       ;<IF-P-BE-VERB <SETG PRSS .OS>>
	       <RETURN .V>)>)>
	<COND (<AND <ZERO? .V>
		    <NOT <IN? <LOC ,WINNER> ,ROOMS>>
		    ;<FSET? <LOC ,WINNER> ,VEHBIT>>
	       <IFFLAG (P-DEBUGGING-PARSER
			<SET V <D-APPLY "M-BEG"
					<GETP <LOC ,WINNER> ,P?ACTION>
					,M-BEG>>)
		       (T
			<SET V <ZAPPLY <GETP <LOC ,WINNER> ,P?ACTION>
				       ,M-BEG>>)>)>
	<COND (<ZERO? .V>
	       <IFFLAG (P-DEBUGGING-PARSER
			<SET V <D-APPLY "M-BEG"
					<GETP ,HERE ,P?ACTION>
					,M-BEG>>)
		       (T
			<SET V <ZAPPLY <GETP ,HERE ,P?ACTION> ,M-BEG>>)>)>
	<COND (<ZERO? .V>
	       <IFFLAG (P-DEBUGGING-PARSER
			<SET V <D-APPLY "Preaction" <ZGET ,PREACTIONS .PA>>>)
		       (T
			<SET V <ZAPPLY <ZGET ,PREACTIONS .PA>>>)>)>
	<SETG NOW-PRSI 1>
	<COND (<AND <ZERO? .V>
		    <T? .PI>
		    <NOT <DIR-VERB?>>
		    <LOC .PI>>
	       <COND (<T? <SET V <GETP <LOC .PI> ,P?CONTFCN>>>
		      <IFFLAG (P-DEBUGGING-PARSER
			       <SET V <D-APPLY "Container" .V ,M-CONTAINER>>)
			      (T
			       <SET V <ZAPPLY .V ,M-CONTAINER>>)>)>)>
	<COND (<AND <ZERO? .V>
		    <T? .PI>>
	       <COND (<EQUAL? .PI ,GLOBAL-HERE>
		      <IFFLAG (P-DEBUGGING-PARSER
			       <SET V <D-APPLY "PRSI" <GETP ,HERE ,P?ACTION>>>)
			      (T
			       <SET V <ZAPPLY <GETP ,HERE ,P?ACTION>>>)>)>
	       <COND (<ZERO? .V>
		      <IFFLAG (P-DEBUGGING-PARSER
			       <SET V <D-APPLY "PRSI" <GETP .PI ,P?ACTION>>>)
			      (T
			       <SET V <ZAPPLY <GETP .PI ,P?ACTION>>>)>)>)>
	<SETG NOW-PRSI 0>
	<COND (<AND <ZERO? .V>
		    <T? .PO>
		    <NOT <DIR-VERB?>>
		    <LOC .PO>>
	       <SET V <GETP <LOC .PO> ,P?CONTFCN>>
	       <COND (<T? .V>
		      <IFFLAG (P-DEBUGGING-PARSER
			       <SET V <D-APPLY "Container" .V ,M-CONTAINER>>)
			      (T
			       <SET V <ZAPPLY .V ,M-CONTAINER>>)>)>)>
	<COND (<AND <ZERO? .V>
		    <T? .PO>
		    <NOT <DIR-VERB?>>>
	       <COND (<EQUAL? .PO ,GLOBAL-HERE>
		      <IFFLAG (P-DEBUGGING-PARSER
			       <SET V <D-APPLY "PRSO" <GETP ,HERE ,P?ACTION>>>)
			      (T
			       <SET V <ZAPPLY <GETP ,HERE ,P?ACTION>>>)>)>
	       <COND (<ZERO? .V>
		      <IFFLAG (P-DEBUGGING-PARSER
			       <SET V <D-APPLY "PRSO" <GETP .PO ,P?ACTION>>>)
			      (T
			       <SET V <ZAPPLY <GETP .PO ,P?ACTION>>>)>)>)>
	<COND (<ZERO? .V>
	       <COND ;<IF-P-BE-VERB>
		     (T
		      <IFFLAG (P-DEBUGGING-PARSER
			       <SET V <D-APPLY <> <ZGET ,ACTIONS .PA>>>)
			      (T
			       <SET V <ZAPPLY <ZGET ,ACTIONS .PA>>>)>)>)>
	<COND (<EQUAL? ,M-FATAL .V>
	       <SETG P-CONT -1>)>
	<SETG PRSA .OA>
	<SETG PRSO .OO>
	<SETG PRSI .OI>
	;<IF-P-BE-VERB <SETG PRSS .OS>>
	.V>

<DEFAULT-DEFINITION TELL-TOO-DARK
 <DEFINE TELL-TOO-DARK () <TELL ,TOO-DARK> <RETURN ,M-FATAL>>>

<DEFAULT-DEFINITION ITAKE-CHECK
 <DEFINE ITAKE-CHECK (OBJ BITS "AUX" (TAKEN <>))
	 <COND (<==? .OBJ ,IT>
		<SET OBJ ,P-IT-OBJECT>)
	       (<==? .OBJ ,THEM>	;"? others?"
		<SET OBJ ,P-THEM-OBJECT>)>
	 <COND (<AND <NOT <HELD? .OBJ ,WINNER>>
		     <NOT <EQUAL? .OBJ ,HANDS ,ROOMS>>>
		<COND (<FSET? .OBJ ,TRYTAKEBIT>
		       T)
		      (<NOT <==? ,WINNER ,PLAYER>>
		       <SET TAKEN T>)
		      (<AND <BTST .BITS ,SEARCH-DO-TAKE>
			    <==? <ITAKE <> .OBJ> T>>
		       <SET TAKEN T>)>
		<COND (<AND <NOT .TAKEN>
			    <BTST .BITS ,SEARCH-MUST-HAVE>
			    <NOT <BTST .BITS ,SEARCH-MOBY>>>
		       <TELL !\[>
		       <COND (<EQUAL? ,WINNER ,PLAYER>
			      <TELL "You are">)
			     (T
			      <TELL-CTHE ,WINNER>
			      <TELL " is">)>
		       <TELL "n't holding ">
		       <TELL-THE .OBJ>
		       <THIS-IS-IT .OBJ>
		       <TELL "!]" CR>
		       <RTRUE>)
		      ;(<AND .TAKEN <==? ,WINNER ,PLAYER>>
		       <FIRST-YOU "take" .OBJ ,ITAKE-LOC>)>)>>>

<IF-P-DEBUGGING-PARSER
<DEFINE TELL-D-LOC (OBJ)
	<PRINTD .OBJ>
	<COND (<IN? .OBJ ,GLOBAL-OBJECTS>	<PRINTI "(gl)">)
	      (<IN? .OBJ ,LOCAL-GLOBALS>	<PRINTI "(lg)">)
	      (<IN? .OBJ ,ROOMS>		<PRINTI "(rm)">)>
	<COND (<EQUAL? .OBJ ,INTNUM>
	       <PRINTC !\(>
	       <PRINTN ,P-NUMBER>
	       <PRINTC !\)>)>>>

<IFFLAG (P-DEBUGGING-PARSER
<DEFINE D-APPLY (STR FCN "OPTIONAL" (FOO <>) "AUX" RES)
 <COND (<T? .FCN>
	<IF-P-DEBUGGING-PARSER
	 <COND (<T? ,P-DBUG>
		<COND (<ZERO? .STR>
		       <PRINTI "{Action:}|">)
		      (T
		       <PRINTC !\{>
		       <PRINT .STR>
		       <COND (<=? .STR "Winner">
			      <PRINTC !\=>
			      <TELL D ,WINNER>)>
		       <PRINTI ": ">)>)>>
	<COND (<T? .FOO> <SET RES <ZAPPLY .FCN .FOO>>)
	      (T <SET RES <ZAPPLY .FCN>>)>
	<IF-P-DEBUGGING-PARSER
	 <COND (<AND <T? ,P-DBUG> <T? .STR>>
		<COND (<OR <EQUAL? ,M-FATAL .RES>
			   <EQUAL? ,P-CONT -1>>
		       <PRINTI "Fatal}|">)
		      (<ZERO? .RES>
		       <PRINTI "Not handled}|">)
		      (T <PRINTI "Handled}|">)>)>>
	.RES)>>)>

<DEFAULT-DEFINITION CAPITAL-NOUN?
	<DEFINE CAPITAL-NOUN? (NAM) <>>>

<DEFAULT-DEFINITION NOT-HERE
<DEFINE NOT-HERE (OBJ "OPT" (CLOCK <>))
	<COND (<ZERO? .CLOCK>
	       <SETG CLOCK-WAIT T>
	       <TELL "[But">)>
	<TELL !\ >
	<TELL-THE .OBJ>
	<IFFLAG (P-BE-VERB
		 <PRINT-IS/ARE .OBJ>)
		(T <TELL " is">)>
	<TELL "n't ">
	<COND (<VISIBLE? .OBJ>
	       <TELL "close enough">
	       <COND (<SPEAKING-VERB?> <TELL " to hear you">)>
	       <TELL !\.>)
	      (T <TELL "here!">)>
	 <THIS-IS-IT .OBJ>
	 <COND (<ZERO? .CLOCK>
		<TELL !\]>)>
	 <CRLF>>>

<DEFAULT-DEFINITION ASKING-VERB-WORD?
 <ADD-WORD ASK ASKWORD>
 <ADD-WORD ORDER ASKWORD>
 <ADD-WORD TELL ASKWORD>
 ;<DEFINE ASKING-VERB-WORD? (WD)
 <COND (<EQUAL? .WD ,W?ASK ,W?ORDER ,W?TELL>
	T)>>>

<DEFAULT-DEFINITION SPEAKING-VERB?
 <DEFINE SPEAKING-VERB? ("OPT" (A ,PRSA) ;(PER 0))
 <COND (<EQUAL? .A ,V?ANSWER ,V?ASK-ABOUT ,V?ASK-FOR ,V?HELLO
		   ,V?NO ,V?REPLY ,V?TELL ,V?TELL-ABOUT ,V?YES>
	<COND (T ;<EQUAL? .PER 0 ,PRSO>
	       <RTRUE>)>)>>>

<DEFINE GET-OWNER (OBJ "AUX" TMP NP)
 <COND (<SET NP <GET-NP .OBJ>>
	<COND (<OR <SET TMP <NP-OF .NP>>
		   <AND <SET TMP <NP-ADJS .NP>>
			<SET TMP <ADJS-POSS .TMP>>>>
	       <COND (<OBJECT? .TMP>
		      .TMP)
		     ;(T
		      <NOUN-PHRASE-OBJ1 .TMP>)>)
	      (<AND <SET TMP <GETP .OBJ ,P?OWNER>>
		    <NOT <OBJECT? .TMP>>>	;"body part"
	       ,PLAYER)>)>>

<DEFINE GET-NP ("OPT" (OBJ <>) "AUX" (PRSI? ,NOW-PRSI))
  <COND (<NOT <EQUAL? .OBJ <> ,PRSO ,PRSI>>
	 <RETURN <>>)
	(.OBJ
	 <COND (<==? .OBJ ,PRSO> <SET PRSI? <>>)
	       (T <SET PRSI? T>)>)>
  <COND (,OBJ-SWAP
	 <COND (<T? .PRSI?> ,PRSO-NP)
	       (T ,PRSI-NP)>)
	(<T? .PRSI?>
	 ,PRSI-NP)
	(T ,PRSO-NP)>>

<DEFAULT-DEFINITION NOUN-USED?
<DEFINE NOUN-USED? (OBJ "OPT" (WD1 <>) (WD2 <>) (WD3 <>) "AUX" X)
   <COND (<AND <SET X <GET-NP .OBJ>>
	       <SET X <NP-NAME .X>>>
	  <COND (<NOT .WD1>
		 .X)
		(<EQUAL? .X .WD1 .WD2 .WD3>
		 T)>)>>>

<DEFAULT-DEFINITION ADJ-USED?
<DEFINE ADJ-USED? (OBJ WD1 "OPT" (WD2 <>) (WD3 <>) "AUX" (NP <GET-NP .OBJ>) CT)
 <COND (<SET NP <NP-ADJS .NP>>
	<COND (<AND <EQUAL? ,PLAYER <ADJS-POSS .NP>>
		    <EQUAL? ,W?MY .WD1 .WD2 .WD3>>
	       ,W?MY)
	      (<G? <SET CT <ADJS-COUNT .NP>> 0>
	       <SET NP <REST-TO-SLOT .NP ADJS-COUNT 1>>
	       <COND (<ZMEMQ .WD1 .NP .CT>
		      .WD1)
		     (.WD2
		      <COND (<ZMEMQ .WD2 .NP .CT>
			     .WD2)
			    (.WD3
			     <COND (<ZMEMQ .WD3 .NP .CT>
				    .WD3)>)>)>)
	      ;(<NOT <ASSIGNED? WD1>>
	       <>)
	      (<EQUAL? .WD1 <>>
	       T)>)
       ;(<NOT <ASSIGNED? WD1>>
	<>)
       (<EQUAL? .WD1 <>>
	T)>>>

<END-SEGMENT>
