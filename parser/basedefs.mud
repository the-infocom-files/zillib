<DEFINITIONS "basedefs">

;<COND (<GASSIGNED? ZILCH!-PACKAGE>
       <USE "ZILCH">)
      (T
       <USE "ZIL">)>

;<USE "NEWSTRUC">

<BLOCK (<ROOT>)>
NUMBER-WORD-CLASSES
GET-CLASSIFICATION
VERB-STUFF-ID
<ENDBLOCK>

<INCLUDE "symbols">

; "Oddly enough, you have to do more than change this to cause winnage..."
<MSETG MAX-VERB-ARGS 2>

<DEFMAC ZILCH? ()
  '<COMPILATION-FLAG-VALUE "IN-ZILCH">>

<DEFMAC CHTYPE-VAL (ATOM TYPE)
  <COND (<ZILCH?>
	 <CHTYPE .ATOM LVAL>)
	(T
	 <FORM SET .ATOM <FORM CHTYPE <CHTYPE .ATOM LVAL> .TYPE>>)>>

<DEFINE GET-TYPE (NUM:FIX "AUX" (L:LIST ,ALL-TERMINALS))
  <MAPF <>
    <FUNCTION (S:SYMBOL)
      <COND (<==? .NUM <SYM-NUMBER .S>>
	     <MAPLEAVE <SYM-NAME .S>>)>>
    .L>>

"Defaults for ZIL-type DEFSTRUCTs"
<SET-DEFSTRUCT-FILE-DEFAULTS ('START-OFFSET 0) ('NTH ZGET) ('PUT ZPUT)
			'NODECL ('PRINTTYPE TABLE-PRINT)>

;<SETG SIBREAKS ".,\"'!?">

<DEFMAC FIND-WORD (WD:STRING "AUX" (NM <STACK <STRING "W?" .WD>>))
  <CHTYPE <OR <LOOKUP .NM <MOBLIST INITIAL>>
	      <INSERT .NM <MOBLIST INITIAL>>>
	  GVAL>>

<DEFMAC COMPARE-WORD-TYPES ('T1 'T2)
  <FORM IFFLAG
	("IN-ZILCH"
	 <COND (<G? ,NUMBER-WORD-CLASSES:FIX 15>
		<FORM AND <FORM ==? <FORM ANDB .T1 *100000*>
			   <FORM ANDB .T2 *100000*>>
		     <FORM NOT
			   <FORM 0? <FORM ANDB .T1 .T2 *77777*>>>>)
	       (T
		<FORM NOT <FORM 0? <FORM ANDB .T1 .T2>>>)>)
	(T
	 <FORM ZIL-COMPARE-WORD-TYPES .T1 .T2>)>>

<DEFINE ZIL-COMPARE-WORD-TYPES (T1:FIX T2:FIX)
  <COND (<G? ,NUMBER-WORD-CLASSES:FIX 15>
	 <AND <0? <ANDB <XORB .T1 .T2> *100000*>>
	      <NOT <0? <ANDB .T1 .T2 *77777*>>>>)
	(T
	 <NOT <0? <ANDB .T1 .T2>>>)>>

<DEFMAC GET-CLASSIFICATION ('TYPE)
  <COND (<AND <TYPE? .TYPE ATOM>
	      <GASSIGNED? ALL-TERMINALS>
	      <NOT <EMPTY? ,ALL-TERMINALS>>>
	 <REALLY-GET-CLASSIFICATION .TYPE>)
	(T
	 <FORM REALLY-GET-CLASSIFICATION .TYPE>)>>

<DEFINE REALLY-GET-CLASSIFICATION (TYPE:<OR ATOM STRING>
				   "AUX" (L:LIST ,ALL-TERMINALS)
				   (RES <>))
  <COND (<TYPE? .TYPE ATOM>
	 <SET TYPE <SPNAME .TYPE>>)>
  <MAPF <>
    <FUNCTION (S:SYMBOL)
      <COND (<=? .TYPE <SPNAME <SYM-NAME .S>>>
	     <SET RES <SYM-NUMBER .S>>
	     <MAPLEAVE>)>>
    .L>
  <COND (<NOT .RES>
	 <ERROR UNKNOWN-CLASSIFICATION!-ERRORS .TYPE GET-CLASSIFICATION>)
	(T
	 .RES)>>

<DEFMAC REST-TO-SLOT ('OBJ WHICH:ATOM "OPT" (PLUS:FIX 0) "AUX" VAL)
  <COND (<GASSIGNED? .WHICH>
	 <COND (<TYPE? <SET VAL ,.WHICH> FIX>)
	       (<TYPE? .VAL OFFSET>
		<SET VAL <INDEX .VAL>>)
	       (<TYPE? .VAL MACRO>
		<SET VAL <EXPAND <FORM .VAL T>>>
		<COND (<TYPE? .VAL ADECL>
		       <SET VAL <1 .VAL>>)>
		<COND (<TYPE? <1 .VAL:FORM> FIX>
		       <SET VAL <1 .VAL:FORM>>)
		      (T
		       <SET VAL <3 .VAL:FORM>>)>)>
	 <SET VAL <+ .VAL:FIX .PLUS>>
	 <COND (<0? .VAL:FIX>
		.OBJ)
	       (T
		<FORM ZREST .OBJ <* .VAL:FIX 2>>)>)
	(T
	 <ERROR UNKNOWN-SLOT!-ERRORS .WHICH REST-TO-SLOT>)>>

; "Form is
<VERSION? (ZIP ...) (XZIP ...) (T ...)>
If `predicate' part is not string or atom, generates cond for that
clause only.  In muddle, just generates cond for the whole thing..."
;<BIND ((REDEFINE T))>
<DEFMAC VERSION? ("ARGS" LIST "AUX" (FIRST? T))
  <COND
   (<ZILCH?>
    <MAPR <>
     <FUNCTION (LL "AUX" (L:LIST <1 .LL>) (TST <1 .L>))
       <COND (<TYPE? .TST ATOM>
	      <SET TST <SPNAME .TST>>)>
       <COND (<AND <TYPE? .TST STRING>
		   <MEMBER .TST '["ZIP" "EZIP" "XZIP" "YZIP"]>>
	      <COND (<CHECK-VERSION? .TST>
		     <COND (<==? <LENGTH .L> 2>
			    <MAPLEAVE <2 .L>>)
			   (T
			    <MAPLEAVE
			     <FORM BIND ()
			       !<REST .L>>>)>)>)
	     (<TYPE? .TST FORM LVAL GVAL>
	      <MAPLEAVE <FORM COND .L>>)
	     (T
	      <COND (<==? <LENGTH .L> 2>
		     <MAPLEAVE <2 .L>>)
		    (T
		     <MAPLEAVE <FORM BIND () !<REST .L>>>)>)>
       <COND (<LENGTH? .LL 1>
	      <MAPLEAVE T>)>>
     .LIST>)
   (T
    <CHTYPE
     <MAPF ,LIST
	  <FUNCTION (L "AUX" (TST <1 .L>))
	     <COND (<TYPE? .TST ATOM>
		    <SET TST <SPNAME .TST>>)>
	     <COND (<AND <TYPE? .TST STRING>
			 <MEMBER .TST '["ZIP" "EZIP" "XZIP" "YZIP"]>>
		    <COND (.FIRST?
			   <SET FIRST? <>>
			   <MAPRET
			    COND
			    <LIST <FORM CHECK-VERSION? .TST> !<REST .L>>>)
			  (T
			   <LIST <FORM CHECK-VERSION? .TST> !<REST .L>>)>)
		   (T
		    <COND (.FIRST?
			   <SET FIRST? <>>
			   <MAPRET COND .L>)
			  (T .L)>)>>
	   .LIST>
     FORM>)>>
;<>

<DEFMAC IF-MUDDLE ('MUDDLE "OPT" ('ZILCH T))
  <COND (<ZILCH?>
	 .ZILCH)
	(T .MUDDLE)>>

<DEFMAC DEBUG-CHECK ('PRED "ARGS" STUFF)
  <COND (<GASSIGNED? ZILCH!-ZILCH!-PACKAGE> T)
	(T
	 <FORM COND (.PRED !.STUFF)>)>>

<DEFMAC DEBUG20 ("ARGS" ARG)
  <COND (<GASSIGNED? ZILCH!-ZILCH!-PACKAGE>
	 T)
	(T
	 <FORM PROG () !.ARG>)>>

<DEFMAC TABLE? ('FOO)
  <COND (<GASSIGNED? ZILCH!-ZILCH!-PACKAGE>
	 <FORM NOT <FORM 0? <FORM ANDB .FOO *177400*>>>)
	(T
	 <FORM TYPE? .FOO TABLE>)>>

<DEFSTRUCT LEXV (TABLE 'NOTYPE 'PRINTTYPE)
	   (LEXV-HEADER FIX 'NONE)	; "dummy"
	   (LEXV-LENGTH FIX 'OFFSET 0 'NTH GETB 'PUT PUTB)
	   (LEXV-NWORDS FIX 'OFFSET 1 'NTH GETB 'PUT PUTB)
	   (LEXV-START VWORD)
	   (LEXV-WORD <OR VWORD FALSE '0> 'OFFSET 0 'NONE)
	   (LEXV-WORD-LENGTH FIX 'OFFSET 2 'NTH GETB 'PUT PUTB 'NONE)
	   (LEXV-WORD-OFFSET FIX 'OFFSET 3 'NTH GETB 'PUT PUTB 'NONE)>

<MSETG LEXV-ELEMENT-SIZE 2>
<MSETG LEXV-ELEMENT-SIZE-BYTES 4>

"The following is a first attempt to make words work in zilch and zil.
If ,ZILCH-RUNNING? is T (meaning that we're compiling something), then
the random VWORD frobbers want to expand with different offsets, depending
on how many bytes are taken up at the front of a word structure by the 
word itself.  In ZIP mode, it's 4; in EZIP, 6."

<MSETG ZIP-ZWORD-BYTES 4>
<MSETG EZIP-ZWORD-BYTES 6>

<NEWTYPE VWORD TABLE>
; "A VWORD has slightly different forms depending on ZIP/EZIP.  In EZIP,
we have
LEXICAL-WORD (string in ZIL, 6 bytes in ZIP)
SEMANTIC-STUFF (2 BYTES)
FLAGS (0 or 2 bytes)
CLASSIFICATION-NUMBER (1 or 2 bytes, parts of speech)
The semantic-stuff is heavily overloaded.  If it's non-zero for a buzzword
(classification number is 0), it points to a word of which this word is a
synonym.  For a verb, it's the pointer to the verb data structure.  For a
direction, the high byte is the direction ID (thus a word can't be both a
verb and a direction).  In all other cases, it's either 0 or a pointer to
a related word (e.g., the singular word of which this is the plural).

For ZIP, the first three fields are the same, except that the LEXICAL-WORD
is four bytes instead of 6.  The semantic-stuff is less overloaded, in that
it never contains verb data.  Instead, that's in an extra slot at the end.
The semantic-stuff has all the same properties, except that the low byte
is the adjective ID, if present."

<DEFMAC WORD-LEXICAL-WORD ('WD "OPT" 'NEW)
  <COND (<ASSIGNED? NEW>
	 <COND (<ZILCH?>
		<ERROR ATTEMPT-TO-CHANGE-WORD-NAME!-ERRORS
		       WORD-LEXICAL-WORD>)>
	 <FORM ZPUT .WD 0 .NEW>)
	(T
	 <FORM ZGET .WD 0>)>>

<DEFINE GET-OFFS (OFFS)
  <COND (<ZILCH?>
	 <+ <VERSION? (ZIP </ ,ZIP-ZWORD-BYTES 2>)
		      (T </ ,EZIP-ZWORD-BYTES 2>)>
	    .OFFS:FIX>)
	(T
	 <+ .OFFS:FIX 1>)>>

<DEFMAC WORD-CLASSIFICATION-NUMBER ('WD "OPT" 'NEW "AUX" OFFS)
  <COND (<NOT <ZILCH?>>
	 <SET OFFS <GET-OFFS 2>>)
	(<COMPILATION-FLAG-VALUE WORD-FLAGS-IN-TABLE>
	 <SET OFFS <GET-OFFS 1>>)
	(T
	 <SET OFFS <GET-OFFS 2>>)>
  <SET WD <CHTYPE [.WD VWORD] ADECL>>
  <COND (<OR <NOT <ZILCH?>>
	     <NOT <COMPILATION-FLAG-VALUE ONE-BYTE-PARTS-OF-SPEECH>>>
	 <COND (<ASSIGNED? NEW>
		<FORM ZPUT .WD .OFFS <CHTYPE [.NEW FIX] ADECL>>)
	       (T
		<CHTYPE [<FORM ZGET .WD .OFFS> FIX] ADECL>)>)
	(<ASSIGNED? NEW>
	 <FORM PUTB .WD <* .OFFS 2> <CHTYPE [.NEW FIX] ADECL>>)
	(T
	 <FORM COND (<FORM 0? <FORM ANDB <FORM GETB .WD <* .OFFS 2>> *200*>>
		     <CHTYPE [<FORM GETB .WD <* .OFFS 2>> FIX] ADECL>)
	       (T
		<CHTYPE [<FORM LSH <FORM ANDB <FORM GETB .WD <* .OFFS 2>>
					 *177*> 7> FIX] ADECL>)>)>>

<DEFMAC WORD-FLAGS ('WD "OPT" 'NEW "AUX" OFFS)
  <SET OFFS <GET-OFFS 1>>
  <COND (<AND <COMPILATION-FLAG-VALUE WORD-FLAGS-IN-TABLE>
	      <ZILCH?>>
	 <FORM BIND (X)
	   <FORM COND (<FORM SET X <FORM INTBL? .WD
					 <FORM REST
					       ',WORD-FLAG-TABLE 2>
					 <FORM ZGET
					       ',WORD-FLAG-TABLE 0>
					 *204*>>
		       <COND (<ASSIGNED? NEW>
			      <FORM ZPUT '.X 1 .NEW>)
			     (T
			      <FORM ZGET '.X 1>)>)
		 (T <>)>>)
	(T
	 <SET WD <CHTYPE [.WD VWORD] ADECL>>
	 <COND (<ASSIGNED? NEW>
		<FORM ZPUT .WD .OFFS <CHTYPE [.NEW FIX] ADECL>>)
	       (T
		<CHTYPE [<FORM ZGET .WD .OFFS> FIX] ADECL>)>)>>

<DEFMAC WORD-SEMANTIC-STUFF ('WD "OPT" 'NEW "AUX" OFFS)
  <SET OFFS <GET-OFFS 0>>
  <SET WD <CHTYPE [.WD VWORD] ADECL>>
  <COND (<ASSIGNED? NEW>
	 <FORM ZPUT .WD .OFFS .NEW>)
	(T
	 <FORM ZGET .WD .OFFS>)>>

<DEFMAC WORD-VERB-STUFF ('WD "OPT" 'NEW "AUX" OFFS)
  <SET WD <CHTYPE [.WD VWORD] ADECL>>
  <COND (<ZILCH?>
	 <VERSION?
	  (ZIP
	   <SET OFFS <GET-OFFS 3>>
	   <COND (<ASSIGNED? NEW>
		  <FORM ZPUT .WD .OFFS <CHTYPE [.NEW '<OR VERB-DATA VERB-POINTER FALSE>]
					       ADECL>>)
		 (T
		  <CHTYPE [<FORM ZGET .WD .OFFS> '<OR FALSE VERB-DATA
						      VERB-POINTER>]
			  ADECL>)>)
	  (T
	   <COND (<ASSIGNED? NEW>
		  <FORM WORD-SEMANTIC-STUFF .WD
			<CHTYPE [.NEW '<OR FALSE VERB-DATA
					   VERB-POINTER>] ADECL>>)
		 (T
		  <CHTYPE [<FORM WORD-SEMANTIC-STUFF .WD>
			   '<OR FALSE VERB-DATA VERB-POINTER>] ADECL>)>)>)
	(T
	 <FORM VERSION?
	       (ZIP
		<COND (<ASSIGNED? NEW>
		       <FORM ZPUT .WD 4 .NEW>)
		      (T
		       <FORM ZGET .WD 4>)>)
	       (T
		<COND (<ASSIGNED? NEW>
		       <FORM WORD-SEMANTIC-STUFF .WD .NEW>)
		      (T
		       <FORM WORD-SEMANTIC-STUFF .WD>)>)>)>>

<DEFMAC WORD-DIR-ID ('WD "OPT" 'NEW "AUX" OFFS)
  <SET WD <CHTYPE [.WD VWORD] ADECL>>
  <COND (<ZILCH?>
	 <VERSION? (ZIP
		    <SET OFFS <+ ,ZIP-ZWORD-BYTES
				 4>>)
		   (T
		    <SET OFFS <+ ,EZIP-ZWORD-BYTES 0>>)>)
	(T
	 <SET OFFS 2>)>
  <COND (<ASSIGNED? NEW>
	 <FORM PUTB .WD .OFFS <CHTYPE [.NEW '<OR FALSE STRING FIX>] ADECL>>)
	(T
	 <CHTYPE [<FORM GETB .WD .OFFS> '<OR FALSE STRING FIX>] ADECL>)>>

<DEFINE DO-WORD-ADJ-ID (WD:VWORD "OPT" NEW:<OR FALSE STRING FIX>)
  <VERSION? (ZIP T)
	    (T
	     <ERROR WORD-ADJ-ID-USED-IN-EZIP/XZIP!-ERRORS .WD>)>
  <COND (<ASSIGNED? NEW>
	 <PUTB .WD 1 .NEW>)
	(T
	 <GETB .WD 1>:<OR FALSE STRING FIX>)>>

<DEFMAC WORD-ADJ-ID ('WD "OPT" 'NEW "AUX" OFFS)
  <COND
   (<ZILCH?>
    <BIND ()
     <VERSION? (ZIP T)
	       (T
		<FORM ERROR WORD-ADJ-ID-USED-IN-EZIP/XZIP!-ERRORS .WD>)>
     <SET OFFS <+ ,ZIP-ZWORD-BYTES 0>>
     <SET WD <CHTYPE [.WD VWORD] ADECL>>
     <COND (<ASSIGNED? NEW>
	    <FORM PUTB .WD .OFFS
		       <CHTYPE <VECTOR .NEW '<OR FALSE STRING FIX>>
			     ADECL>>)
	   (T
	    <CHTYPE <VECTOR <FORM GETB .WD .OFFS>
			       '<OR FALSE STRING FIX>> ADECL>)>>)
   (<ASSIGNED? NEW>
    <FORM DO-WORD-ADJ-ID .WD .NEW>)
   (T
    <CHTYPE <VECTOR <FORM DO-WORD-ADJ-ID .WD> '<OR FALSE STRING FIX>> ADECL>)>>

<DEFINE VWORD-PRINT (V:VWORD "AUX" (SEM <WORD-SEMANTIC-STUFF .V>)
		     (OUTCHAN .OUTCHAN))
	<PRINC <WORD-LEXICAL-WORD .V>>
	<COND
	 (<0? <WORD-CLASSIFICATION-NUMBER .V>>
	  <COND (<AND .SEM <N==? .SEM 0>>
		 <PRINT-MANY .OUTCHAN PRINC ":SYN:"
			     <WORD-LEXICAL-WORD .SEM>>)
		(T
		 <PRINC ":BUZZ">)>)
	 (T
	  <REPEAT ((WD <WORD-CLASSIFICATION-NUMBER .V>)
		   (CT 1) (MSK 1) (BIT <ANDB .WD *100000*>))
		  <COND (<NOT <0? <ANDB .MSK .WD>>>
			 <COND (<==? <ORB .BIT .MSK> <GET-CLASSIFICATION ADJ>>
				<VERSION? (ZIP
					   <SET SEM <>>
					   <PRINT-MANY .OUTCHAN PRINC !\:
					     "ADJ" !\? <WORD-ADJ-ID .V>>)
					  (T
					   <PRINC ":ADJ">)>)
			       (<==? <ORB .BIT .MSK> <GET-CLASSIFICATION DIR>>
				<SET SEM <>>
				<PRINT-MANY .OUTCHAN PRINC !\:
					    "DIR" !\? <WORD-DIR-ID .V>>)
			       (T
				<PRINT-MANY .OUTCHAN PRINC !\:
				     <SPNAME <GET-TYPE <ORB .BIT .MSK>>>>)>)>
		  <COND (<G? <SET CT <+ .CT 1>> 15>
			 <RETURN>)>
		  <SET MSK <LSH .MSK 1>>>)>
	<VERSION? (ZIP
		   <COND (<WORD-VERB-STUFF .V>
			  <PRINC !\:>
			  <PRIN1 <WORD-VERB-STUFF .V>>)>)>
	<COND (<AND .SEM <N==? .SEM 0>> 
	       <PRINC !\:>
	       <PRIN1 .SEM>)>>

<COND (<GASSIGNED? VWORD-PRINT> <PRINTTYPE VWORD ,VWORD-PRINT>)>

<DEFINE MAKE-VWORD (NAM CLASS:FIX FLAGS:FIX "AUX" TAB)
  <VERSION? (ZIP <SET TAB <CHTYPE <ITABLE 5 (TEMP-TABLE)> VWORD>>)
	    (T <SET TAB <CHTYPE <ITABLE 4 (TEMP-TABLE)> VWORD>>)>
  <WORD-LEXICAL-WORD .TAB .NAM>
  <WORD-CLASSIFICATION-NUMBER .TAB .CLASS>
  <WORD-FLAGS .TAB .FLAGS>
  <WORD-SEMANTIC-STUFF .TAB <>>
  <VERSION? (ZIP
	     <WORD-VERB-STUFF .TAB <>>)>
  .TAB>

; "We may want to add something here to allow verb types (hurtverbs,
etc)."
<NEWTYPE VERB-POINTER ATOM>

<DEFSTRUCT VERB-DATA
	   (TABLE
	    ('INIT-ARGS (TEMP-TABLE)))
	   ; "If GE 0, this verb is happy with no arguments.  The
	      number identifies the action routine."
	   (VERB-ZERO ANY -1)
	   ; "VERB-ONE and VERB-TWO contain tables of VERB-SYNTAXES;
	      the correct
	      one is picked based on prepositions in the sentence, matched
	      against stuff in the SYNTAX element.  This determines the
	      allowable prepositions for the verb, and also the action routines
	      to call."
	   (VERB-PREP <OR FALSE FIX VWORD>)
	   (VERB-ONE <OR FALSE FIX TABLE>)
	   (VERB-TWO <OR FALSE FIX TABLE>)>

<NEWTYPE VERB-SYNTAX VECTOR>
<COND (<GASSIGNED? TABLE-PRINT>
       <PRINTTYPE VERB-SYNTAX ,TABLE-PRINT>)>

<MSETG VERB-ONE-SYNTAX-LEN 6>
<MSETG VERB-TWO-SYNTAX-LEN 10>

; "All the syntaxes for a verb are stored in two tables, one for the one arg
   case, the other for the two arg case.  Each table contains a length
   word, the number of entries in the table, and N entries, either 6 or 10
   bytes long.  The one arg entries are:
   action ID (2 bytes) ? preposition or false (2 bytes) ? FIND ? SEARCH
   Two arg entries repeat the last four bytes.  The following macros
   assume that they're dealing with a table rested to the beginning of a
   syntax entry."

; "Get the syntax-id out"
<DEFMAC SYNTAX-ID ('SYN "OPT" 'NEW)
  <COND (<NOT <ASSIGNED? NEW>>
	 <FORM ZGET <CHTYPE [.SYN VERB-SYNTAX] ADECL> 0>)
	(T
	 <FORM ZPUT <CHTYPE [.SYN VERB-SYNTAX] ADECL> 0 .NEW>)>>

<MSETG SYN-PREP 0>
<MSETG SP-LOCATION-TYPE -1>
<MSETG SYN-FIND 2>		; "Byte offset"
<MSETG SYN-FIND-NEGATE *200*>	; "If set, try something with this bit off"
<MSETG SYN-SEARCH 3>		; "Byte offset"
<MSETG SYN-FIRST-ENTRY 1>	; "Word offset"
<MSETG SYN-ENTRY-SIZE 2>	; "Word length"

<DEFMAC SYNTAX-GET ('VS ELT 'WHICH)
  <COND (<TYPE? .ELT ATOM>
	 <SET ELT ,.ELT>)>
  <COND (<==? .ELT ,SYN-PREP>
	 ; "This is a word."
	 <FORM ZGET .VS
	       <FORM + .ELT ,SYN-FIRST-ENTRY
		     <FORM * <FORM - .WHICH 1> ,SYN-ENTRY-SIZE>>>)
	(T
	 ; "Everything else is bytes"
	 <FORM GETB .VS
	       <FORM + .ELT <* 2 ,SYN-FIRST-ENTRY>
		     <FORM * <FORM - .WHICH 1> <* ,SYN-ENTRY-SIZE 2>>>>)>>

; "Macros to get stuff out of the syntax slot of a VERB-SYNTAX.  Second arg
   will be evaluated at macro expansion time, so must be a constant..."
<DEFMAC SYNTAX-PREP ('VS "OPT" (WHICH:FIX 1))
  <FORM ZGET <CHTYPE [.VS VERB-SYNTAX] ADECL>
	<+ ,SYN-PREP ,SYN-FIRST-ENTRY <* <- .WHICH 1> ,SYN-ENTRY-SIZE>>>>

<DEFMAC SYNTAX-FIND ('VS "OPT" (WHICH:FIX 1))
  <FORM GETB <CHTYPE [.VS VERB-SYNTAX] ADECL>
	<+ ,SYN-FIND <* 2 ,SYN-FIRST-ENTRY>
	   <* <- .WHICH 1> ,SYN-ENTRY-SIZE 2>>>>

<DEFMAC SYNTAX-SEARCH ('VS "OPT" (WHICH:FIX 1))
  <FORM GETB <CHTYPE [.VS VERB-SYNTAX] ADECL>
	<+ ,SYN-SEARCH <* 2 ,SYN-FIRST-ENTRY>
	   <* <- .WHICH 1> ,SYN-ENTRY-SIZE 2>>>>

; "An action below 128 is a shift; above 128 is a reduction (subtract
   129 to get the (0-based) offset in the reduction table).  128 is
   done."
<MSETG ACTION-SPLIT 128>
<MSETG REDUCTION-OFFSET <+ ,ACTION-SPLIT 1>>

<GDECL (ACTION-TABLE) <TABLE FIX [REST <VECTOR <OR FALSE VECTOR>
					       <OR FALSE VECTOR>>]>
       (REDUCTION-TABLE) <TABLE [REST REDUCTION]>>

<DEFSTRUCT REDUCTION
	   (TABLE
	    ('PRINTTYPE PRINT-REDUCTION)
	    ('INIT-ARGS (PARSER-TABLE)))
	   (REDUCTION-SIZE FIX)
	   (REDUCTION-FUNCTION <OR ATOM APPLICABLE>)
	   (REDUCTION-ERR-PRIORITY FIX)
	   (REDUCTION-PRIORITY FIX)
	   (REDUCTION-RESULT FIX)
	   (REDUCTION-NAME STRING)>

<DEFINE PRINT-REDUCTION (R:REDUCTION "AUX" (OUTCHAN .OUTCHAN)
			 (FUNC <REDUCTION-FUNCTION .R>))
	<PRINC "#REDUCTION [">
	<PRIN1 <NTH ,ALL-NONTERMINALS <REDUCTION-RESULT .R>>>
	<PRINC !\ >
	<PRINC <REDUCTION-NAME .R>>
	<PRINC !\ >
	<PRIN1 <REDUCTION-SIZE .R>>
	<PRINC !\ >
	<PRIN1 <REDUCTION-ERR-PRIORITY .R>>
	<PRINC !\ >
	<PRIN1 <REDUCTION-PRIORITY .R>>
	<PRINC !\]>>

<END-DEFINITIONS>
