"FIND file for NEW PARSER
Copyright (C) 1988 Infocom, Inc.  All rights reserved."

<ZZSECTION "find">

<INCLUDE "basedefs" "pdefs" "pbitdefs">

<USE ;"NEWSTRUC" "parser" "pmem">

<BLOCK (<ROOT>)>
THINGS
<ENDBLOCK>

<FILE-FLAGS MDL-ZIL? CLEAN-STACK? ;ZAP-TO-SOURCE-DIRECTORY?>

<DEFAULTS-DEFINED
	EXCLUDE-HERE-OBJECT?
	INVALID-OBJECT?
	MOBY-FIND?
	LAST-PSEUDO-LOC
	PSEUDO-OBJECTS
	SEARCH-IN-LG?>

<BEGIN-SEGMENT 0>

<PUT-DECL BOOLEAN '<OR ATOM FALSE>>

<DEFMAC FD-FLAG (WHICH 'VAL "OPT" 'NEW)
  <COND (<ASSIGNED? NEW>
	 <COND (<OR <TYPE? .NEW ATOM FALSE>
		    <AND <TYPE? .NEW FORM>
			 <EMPTY? .NEW>>>
		<COND (<TYPE? .NEW ATOM>
		       ;"Just turning flag on"
		       <FORM ORB ,.WHICH .VAL>)
		      (T
		       <FORM ANDB .VAL <XORB ,.WHICH -1>>)>)
	       (<TYPE? .VAL FIX LVAL GVAL>
		<FORM COND
		      (.NEW
		       <FORM ORB .VAL ,.WHICH>)
		      (T
		       <FORM ANDB .VAL <XORB ,.WHICH -1>>)>)
	       (T
		<FORM BIND ((FLAG .VAL))
		  <FORM COND
			(.NEW
			 <FORM ORB ,.WHICH '.FLAG>)
			(T
			 <FORM ANDB '.FLAG <XORB ,.WHICH -1>>)>>)>)
	(T
	 <FORM NOT <FORM 0? <FORM ANDB .VAL ,.WHICH>>>)>>

<MSETG FIND-FLAGS-GWIM 1>
<DEFMAC FIND-GWIM? ('F)
  <FORM NOT <FORM 0? <FORM ANDB <FORM FIND-FLAGS .F> ,FIND-FLAGS-GWIM>>>>

<CONSTANT FINDER <MAKE-FINDER>>
<GLOBAL P-NOT-HERE:NUMBER ;BYTE 0>

"FIND-DESCENDANTS, MATCH-OBJECT, and ADD-OBJECT all return false when the
 search should be stopped prematurely because some object was an exact
 match.  If there's a big red book and a big ugly red book, BIG RED BOOK
 will get the former, since it's the only way to do so."

<DEFINE FIND-DESCENDANTS FD
	(PARENT:OBJECT FLAGS:FIX ;"INCLUDE, SEARCH, NEST, NOTOP"
	 "AUX" (F ,FINDER) FOBJ:<OR FALSE OBJECT>)
  <COND (<EQUAL? .PARENT ,GLOBAL-HERE>
	 <SET PARENT ,HERE>)>
  <COND (<SET FOBJ <FIRST? .PARENT>>
	 ;"This guy contains something"
	 <REPEAT ()
	   ;"See if the current object matches: if so, add it to the list"
	   <COND
	    (<VISIBLE? .FOBJ>
	     <COND (<AND <NOT <FD-FLAG FD-NOTOP? .FLAGS> ;<BTST .FLAGS 8>>
			 <NOT <MATCH-OBJECT .FOBJ .F
					    <FD-FLAG FD-INCLUDE? .FLAGS>
					    ;<BTST .FLAGS 1>>>>
		    <RETURN <> .FD>)>
	     <COND (<AND <FD-FLAG FD-NEST? .FLAGS> ;<BTST .FLAGS 4>
			 <FIRST? .FOBJ>
			 <N==? .FOBJ ,WINNER>
			 <OR ;,P-MOBY-FLAG
			     <AND <FSET? .FOBJ ,SEARCHBIT>
				  <OR <FSET? .FOBJ ,OPENBIT>
				      <FSET? .FOBJ ,TRANSBIT>>>
			     <FSET? .FOBJ ,SURFACEBIT>>>
		    ;"Check its contents"
		    <COND (<NOT <FIND-DESCENDANTS .FOBJ
				 <FD-FLAG FD-INCLUDE? ,FD-NEST?
					  <FD-FLAG FD-INCLUDE? .FLAGS>>
				 ;<COND (<BTST .FLAGS 1> 5) (T 4)>>>
			   <RETURN <> .FD>)>)>)>
	   ;"Check next sibling"
	   <COND (<NOT <SET FOBJ <NEXT? .FOBJ>>>
		  <RETURN T .FD>)>>)
	(T)>>

<DEFINE EXCLUDED? EX (FOBJ:OBJECT F:FINDER
			  "AUX" (EXC:<OR FALSE PMEM> <FIND-EXCEPTIONS .F>))
  <COND (.EXC
	 <REPEAT ((PHRASE:PMEM <NPP-NOUN-PHRASE .EXC>)
		  CT:FIX VEC VV)
	    <COND (.PHRASE
		   ; "We may possibly have a non-existent phrase if
		      the luser was excluding an object that wasn't
		      here.  Be safe, rather than sorry."
		   <SET CT <NOUN-PHRASE-COUNT .PHRASE>>
		   <SET VEC <REST-TO-SLOT .PHRASE NOUN-PHRASE-OBJ1>>
		   <REPEAT ()
		      <COND (<L? <SET CT <- .CT 1>> 0>
			     <SET VV <>>
			     <RETURN>)>
		      <COND (<==? .FOBJ <ZGET .VEC 0>>
			     <SET VV T>
			     <RETURN>)>
		      <SET VEC <ZREST .VEC 4 ;2>>>
		   <COND (.VV
			  <RETURN T .EX>)>)>
	    <COND (<SET EXC <NPP-NEXT .EXC>>
		   <SET PHRASE <NPP-NOUN-PHRASE .EXC>>)
		  (T
		   <RETURN <> .EX>)>>)>>

<DEFAULT-DEFINITION INVALID-OBJECT?
	<DEFINE INVALID-OBJECT? (OBJ) <>>>

<DEFINE MATCH-OBJECT (FOBJ:OBJECT F:FINDER INCLUDE?:BOOLEAN
		      "AUX" NOUN ADJS APP TB (RES <FIND-RES .F>) TMP)
  <COND (<AND <NOT <FSET? .FOBJ ,INVISIBLE>>
	      <OR <EQUAL? <SET NOUN <FIND-NOUN .F>> <> ,W?ONE>
		  <AND <SET TB <GETPT .FOBJ ,P?SYNONYM>>
		       <ZMEMQ .NOUN .TB </ <PTSIZE .TB>:FIX 2>>>>
	      <OR <NOT <SET ADJS <FIND-OF .F>>>
		  <CHECK-ADJS .FOBJ .F .ADJS>>
	      <OR <NOT <SET ADJS <FIND-ADJS .F>>>
		  <CHECK-ADJS .FOBJ .F .ADJS>>
	      <NOT <EXCLUDED? .FOBJ .F>>
	      <OR <FIND-GWIM? .F>
		  <NOT <INVALID-OBJECT? .FOBJ>>>>
	 ;"This object matches the words used..."
	 <COND (<NOT .INCLUDE?>	;"location didn't match the syntax bits"
		T)
	       (<AND <T? <SET ADJS <FIND-ADJS .F>>>
		     <SET TMP <GETPT .FOBJ ,P?ADJECTIVE>>
		     ;"TAA 6/23/89  Got confused in Arthur 'cause lots of swords
		       have no adjectives.  Let's require that before we do anything
		       radical."
		     <EQUAL? <ADJS-COUNT .ADJS>
			     <COND (T ;<CHECK-EXTENDED?>
				    </ <PTSIZE .TMP> 2>)
			   ;(T <- <PTSIZE <GETPT .FOBJ ,P?ADJECTIVE>> 1>)>>>
		;"the only way to do so."
		<FIND-RES-COUNT .RES 1>
		<FIND-RES-NEXT .RES <>>
		<FIND-RES-OBJ1 .RES .FOBJ>
		<COND (<EQUAL? .FOBJ ,HERE>
		       <FIND-RES-OBJ1 .RES ,GLOBAL-HERE>)>
		<>)
	       (<AND <T? <SET APP <FIND-APPLIC .F>>>
		     <NOT <FIND-GWIM? .F>>>
		;"We're not GWIMming, so apply the test only if there's an
		   ambiguity"
		<COND (<OR <0? <FIND-RES-COUNT .RES>>
			   <FIND-QUANT .F>>
		       ;"Don't have anything yet"
		       <ADD-OBJECT .FOBJ .F>)
		      (<TEST-OBJECT .FOBJ .APP .F>
		       ;"We already have something, so first find out if
			  this one's OK"
		       <COND (<1? <FIND-RES-COUNT .RES>>
			      ;"There's only one other object"
			      <COND (<NOT <TEST-OBJECT
					       <FIND-RES-OBJ1 .RES>
					       .APP .F>>
				     ;"The other object doesn't match, so just
					replace it"
				     <FIND-RES-OBJ1 .RES .FOBJ>
				     <COND (<EQUAL? .FOBJ ,HERE>
					   <FIND-RES-OBJ1 .RES ,GLOBAL-HERE>)>
				     T)
				    (T
				     ;"The other object also matches, so
					we're stuck"
				     <ADD-OBJECT .FOBJ .F>)>)
			     (T
			      ;"We already have more than one object, so
				 we're losing"
			      <ADD-OBJECT .FOBJ .F>)>)>)
	       (<F? .APP>
		<COND (<OR <NOT <FIND-GWIM? .F>>
			   <FIND-QUANT .F>>	;"DETERMINE-OBJ w/ PICK"
		       <ADD-OBJECT .FOBJ .F>)
		      (T)>)
	       (<TEST-OBJECT .FOBJ .APP .F>
		<ADD-OBJECT .FOBJ .F>)
	       (T)>)
	(T)>>

<MSETG SYN-FIND-PROP *400*>	;"If set, look for this property"

<DEFINE TEST-OBJECT TO (FOBJ:OBJECT APP:<OR FIX TABLE> F:FINDER)
  <COND (<NOT <TABLE? .APP>>
	 <COND (<NOT <0? <ANDB .APP ,SYN-FIND-NEGATE>>>
		<NOT <FSET? .FOBJ <ANDB .APP *77*>>>)
	       (T
		<FSET? .FOBJ .APP>)>)
	(T
	 <COND (<NOT <0? <ANDB <ZGET .APP 1> ,SYN-FIND-PROP>>>
		<COND (<EQUAL? <GETP .FOBJ <ANDB <ZGET .APP 1> *77*>>
			       <ZGET .APP 2>>
		       <RETURN T .TO>)
		      (T <RETURN <> .TO>)>)>
	 <REPEAT ((N:FIX <ZGET .APP 0>) NN)
	   <SET NN <ZGET .APP .N>>
	   <COND (<NOT <0? <ANDB .NN ,SYN-FIND-NEGATE>>>
		  <COND (<NOT <FSET? .FOBJ <ANDB .NN *77*>>>
			 <RETURN T .TO>)>)
		 (<FSET? .FOBJ .NN>
		  <RETURN T .TO>)>
	   <COND (<L? <SET N <- .N 1>> 1>
		  <RETURN <> .TO>)>>)>>

"Object matches all other tests.  Here do checks with quantities
   (all, one, etc.), then add if OK."
<DEFINE ADD-OBJECT (OBJ:OBJECT F:FINDER "AUX" (VEC <FIND-RES .F>) NC
		    (DOIT? T) (SYN <FIND-SYNTAX .F>) (WHICH <FIND-WHICH .F>))
  <COND (<EQUAL? .OBJ ,HERE>
	 <SET OBJ ,GLOBAL-HERE>)>	;"per PDL 29-Apr-88"
  <COND (<AND <NOT <FIND-QUANT .F>>
	      .SYN
	      <==? 1 <FIND-RES-COUNT .VEC>:FIX>>
	 <COND (<MULTIPLE-EXCEPTION? .OBJ .SYN .WHICH .F>
		<SET DOIT? <>>)
	       (<MULTIPLE-EXCEPTION? <FIND-RES-OBJ1 .VEC> .SYN .WHICH .F>
		<FIND-RES-OBJ1 .VEC .OBJ>
		<SET DOIT? <>>)>)>
  <COND (<AND .DOIT?
	      <OR <NOT <FIND-QUANT .F>>
		  <NOT <FIND-SYNTAX .F>>
		  <NOT <MULTIPLE-EXCEPTION? .OBJ	;"wrong theory of ALL?"
					    <FIND-SYNTAX .F>
					    <FIND-WHICH .F>
					    .F>>>
	      ;"In case an object gets found twice..."
	      <SET WHICH <NOT-IN-FIND-RES? .OBJ .VEC>>>
	 <FIND-RES-COUNT .VEC ;<SET NC > <+ 1 <FIND-RES-COUNT .VEC>>>
	 <COND ;(<AND <IN? <SET NC <META-LOC .OBJ>> ,ROOMS>
		     <NOT <EQUAL? .NC <META-LOC ,WINNER>>>>
		<ZPUT .WHICH 0 <- 0 .OBJ>>)	;"adjacent room"
	       (T
		<ZPUT .WHICH 0 .OBJ>)>
	 ;<COND (<L=? .NC <FIND-RES-SIZE .VEC>>
		<ZPUT <REST-TO-SLOT .VEC FIND-RES-OBJ1>
		      <- .NC 1>
		      .OBJ>)>
	 <N==? <FIND-QUANT .F> ,NP-QUANT-A>)
	(T)>>

<DEFINE NOT-IN-FIND-RES? ACT (OBJ VEC "OPT" (NO-CHANGE? <>))
 <REPEAT ((CT <FIND-RES-COUNT .VEC>)
	  (SZ <FIND-RES-SIZE .VEC>) ANS NVEC)
	 <SET ANS <REST-TO-SLOT .VEC FIND-RES-OBJ1>>
	 <COND (<L? .CT 1>
		<RETURN .ANS .ACT>)
	       (<G? .CT .SZ>
		<SET CT <- .CT .SZ>>)
	       (T <SET SZ .CT>)>
	 <COND (<INTBL? .OBJ .ANS .SZ>
		<RETURN <> .ACT>)
	       (<T? <SET NVEC <FIND-RES-NEXT .VEC>>>
		<SET VEC .NVEC>
		<SET SZ ,FIND-RES-MAXOBJ ;<OBJLIST-SIZE .VEC>>)
	       (<L? .SZ ,FIND-RES-MAXOBJ ;<FIND-RES-SIZE .VEC>>
		<RETURN <ZREST .ANS <* 2 .SZ>> .ACT>)
	       (<T? .NO-CHANGE?>
		<RETURN T .ACT>)
	       (T
		<SET SZ ,FIND-RES-MAXOBJ ;<FIND-RES-SIZE .VEC>>
		<SET NVEC <PMEM-ALLOC OBJLIST
				      ;"SIZE .SZ"
				      LENGTH <- ,FIND-RES-LENGTH 1>>>
		<FIND-RES-NEXT .VEC .NVEC>
		<RETURN <REST-TO-SLOT .NVEC FIND-RES-OBJ1> .ACT>)>>>

"EVERYWHERE-VERB? -- separately defined so game can call it"

<DEFINE EVERYWHERE-VERB? ("OPT" (WHICH <FIND-WHICH ,FINDER>)
				(SYNTAX <PARSE-SYNTAX ,PARSE-RESULT>)
			  "AUX" SYN)
	<COND (<==? .WHICH 1>
	       <SET SYN <SYNTAX-SEARCH .SYNTAX 1>>)
	      (T
	       <SET SYN <SYNTAX-SEARCH .SYNTAX 2>>)>
	<COND (<AND <ANDB ,SEARCH-MOBY .SYN>
		    <NOT <ANDB ,SEARCH-MUST-HAVE .SYN>>>
	       T)>>

"MULTIPLE-EXCEPTION? -- return true if an object found by ALL should not
be include when the crunch comes."

<DEFINE MULTIPLE-EXCEPTION? (OBJ:OBJECT SYNTAX:VERB-SYNTAX WHICH:FIX F:FINDER
			     "AUX" (L <LOC .OBJ>) (VB <SYNTAX-ID .SYNTAX>))
 <COND (<EQUAL? .OBJ <> ,ROOMS ;,NOT-HERE-OBJECT>
	<SETG P-NOT-HERE <+ 1 ,P-NOT-HERE>>
	T)
       (<AND <0? <EVERYWHERE-VERB? .WHICH .SYNTAX>>
	     <NOT <ACCESSIBLE? .OBJ>>>
	T)
       (<AND <==? .VB ,V?TAKE>
	     <ZERO? <FIND-NOUN .F>>
	     <1? .WHICH>>
	<COND (<AND <NOT <FSET? .OBJ ,TAKEBIT>>
		    <NOT <FSET? .OBJ ,TRYTAKEBIT>>>
	       T)
	      (<EQUAL? .L ,WINNER>
	       ;<AND <NOT <EQUAL? .L ,WINNER <LOC ,WINNER> ,HERE>>
		    <NOT <FSET? .L ,SURFACEBIT>>
		    <NOT <FSET? .L ,SEARCHBIT>>>
	       T)>)
       (<==? .VB ,V?DROP>
	<COND (<NOT <IN? .OBJ ,WINNER>>
	       T)>)
      ;(<AND ,PRSI
	     <==? ,PRSO ,PRSI>>
	;"VERB ALL and prso = prsi"
	<RTRUE>)
      ;(<AND <==? .VB ,V?PUT>
	     <NOT <IN? .OBJ ,WINNER>>
	     <HELD? ,PRSO ,PRSI>>
	;"PUT ALL IN X and object already in x"
	<RTRUE>)>>

<ADD-WORD OPEN ADJ>
<ADD-WORD CLOSED ADJ>
<ADD-WORD SHUT ADJ>

<DEFINE CHECK-ADJS-THERE? (OWNER "AUX" TMP)
   <COND (<ZERO? <SET TMP <FIND-RES-COUNT ,OWNER-SR-THERE>>>
	  <>)
	 (<NOT <INTBL? .OWNER
		       <REST-TO-SLOT ,OWNER-SR-THERE FIND-RES-OBJ1>
		       .TMP>>
	  <>)
	 (T)>>

<DEFINE CHECK-ADJS CA (OBJ:OBJECT F ADJS:PMEM
		       "AUX" CNT (TMP <>) OWNER (ID <>) VEC)
  <SET OWNER <GETP .OBJ ,P?OWNER>>
  <COND (<OR <PMEM-TYPE? .ADJS NP>	;"it's NP-OF"
	     <SET TMP <ADJS-POSS .ADJS>>>
	 <COND (<OBJECT? <SET ID .OWNER>>
		<COND (<EQUAL? .OWNER .TMP ;.OBJ>
		       T)
		      ;(<EQUAL? .OWNER .OBJ>)
		      (<EQUAL? .OWNER ,ROOMS ;"any">
		       <COND (<FIND-RES-COUNT ,OWNER-SR-HERE>
			      <SET ID <FIND-RES-OBJ1 ,OWNER-SR-HERE>>
			      ;"real owner")
			     (<FIND-RES-COUNT ,OWNER-SR-THERE>
			      <SET ID <FIND-RES-OBJ1 ,OWNER-SR-THERE>>)
			     (T
			      <RETURN <> .CA>)>)
		      (<ZERO? <SET TMP <FIND-RES-COUNT ,OWNER-SR-HERE>>>
		       <COND (<NOT <CHECK-ADJS-THERE? .OWNER>>
			      <RETURN <> .CA>)>)
		      (<NOT <INTBL? .OWNER
				    <REST-TO-SLOT ,OWNER-SR-HERE FIND-RES-OBJ1>
				    .TMP>>
		       <COND (<NOT <CHECK-ADJS-THERE? .OWNER>>
			      <RETURN <> .CA>)>)>)
	       (<T? .OWNER>	;"table for multiple owners (body parts)"
		;<SET ID <>>
		<COND (<AND ;<ZERO? .ID>
			    <ZERO? <SET CNT <FIND-RES-COUNT ,OWNER-SR-HERE>>>
			    ;<SET ID <INTBL? ,PLAYER .TMP <ZGET .OWNER 0>>>>
		       <SET ID ,PLAYER>	;"default owner of body part"
		       ;<SET ID <ZGET .ID 0>>)
		      (T
		       <SET TMP <ZREST .OWNER 2>>
		       <SET VEC <REST-TO-SLOT ,OWNER-SR-HERE FIND-RES-OBJ1>>
		       <REPEAT ()
			<COND (<DLESS? CNT 0>
			       <RETURN <> .CA>)
			      (<SET ID
				<INTBL? <ZGET .VEC 0> .TMP <ZGET .OWNER 0>>>
			       <SET ID <ZGET .ID 0>>
			       <RETURN>)
			      (T <SET VEC <ZREST .VEC 2>>)>>)>)
	       (<OBJECT? .TMP>		;"possession"
		<COND (<NOT <HELD? .OBJ .TMP>>
		       <RETURN <> .CA>)>)
	       (T			;"possession"
		<COND (<ZERO? <SET TMP <FIND-RES-COUNT ,OWNER-SR-HERE>>>
		       <RETURN <> .CA>)
		      (<NOT <SET ID <INTBL? <LOC .OBJ>
				<REST-TO-SLOT ,OWNER-SR-HERE FIND-RES-OBJ1>
				.TMP>>>
		       <RETURN <> .CA>)
		      ;(T <SET ID <ZGET .ID 0>>)>)>)>
  <COND (<NOT <EQUAL? .ID 0 .OBJ>>	;<T? .ID>
	 <FIND-RES-OWNER <FIND-RES .F> .ID>)>
  <COND (<NOT <PMEM-TYPE? .ADJS NP>>
	 <SET VEC <REST-TO-SLOT .ADJS ADJS-COUNT 1>>
  <REPEAT ((CT <ADJS-COUNT .ADJS>) ADJ FL
	   (OADJS <GETPT .OBJ ,P?ADJECTIVE>)
	   (NUM </ <PTSIZE .OADJS>:FIX 2>))
    <COND (<L? <SET CT <- .CT 1>> 0>
	   <RETURN>)>
    <COND
     (T ;<CHECK-EXTENDED?>
      <SET ADJ <ZGET .VEC .CT>>
      <SET ID .ADJ>)
     ;(T
      <COND (<0? <SET ID <WORD-ADJ-ID <SET ADJ <ZGET .VEC .CT>>>>>
	     <COND (<NOT <IF-MUDDLE <COND (<GASSIGNED? SPECIAL-ADJ-CHECK>
					   <SPECIAL-ADJ-CHECK .ADJ .OBJ>)>
				    <SPECIAL-ADJ-CHECK .ADJ .OBJ>>>
		    <RETURN <> .CA>)>)>)>
    <COND (<EQUAL? .ADJ ,W?NO.WORD>
	   <AGAIN>)
	  (<ZMEMQ .ID .OADJS .NUM>
	   ;<COND (T ;<CHECK-EXTENDED?>
		  )
		 ;(T <ZMEMQB .ID .OADJS <- <PTSIZE .OADJS>:FIX 1>>)>)
	  (<AND <EQUAL? .ID ,W?CLOSED ,W?SHUT>
		<NOT <FSET? .OBJ ,OPENBIT>>>)
	  (<AND <EQUAL? .ID ,W?OPEN>
		<FSET? .OBJ ,OPENBIT>>)
	  ;(<VERSION? (ZIP <>)
		     (T
		      <IF-MUDDLE <AND <GASSIGNED? SPECIAL-ADJ-CHECK>
				      <SPECIAL-ADJ-CHECK .ADJ .OBJ>>
				 <SPECIAL-ADJ-CHECK .ADJ .OBJ>>)>)
	  (T
	   <RETURN <> .CA>)>>)>
  T>

<OBJECT GENERIC-OBJECTS
	(ADJACENT 0)	;"to establish property">

<DEFAULT-DEFINITION MOBY-FIND?
;<DEFINE MOBY-FIND? (SEARCH)
	<COND (<OR <AND <NOT <0? <ANDB .SEARCH ,SEARCH-MOBY ;128>>>
			<0? <ANDB .SEARCH ,SEARCH-MUST-HAVE>>>
		   <BAND ,PAST-TENSE <WORD-FLAGS <PARSE-VERB ,PARSE-RESULT>>>>
	       T)>>
<DEFMAC MOBY-FIND? ('SEARCH)
	<FORM OR <FORM AND <FORM BAND .SEARCH ',SEARCH-MOBY ;128>
		  <FORM 0? <FORM BAND .SEARCH ',SEARCH-MUST-HAVE>>>
		 <FORM BAND ',PAST-TENSE
			    <FORM WORD-FLAGS
				  <FORM PARSE-VERB ',PARSE-RESULT>>>>>>

<DEFAULT-DEFINITION SEARCH-IN-LG?
	<DEFINE SEARCH-IN-LG? (OBJ) <>>>

<DEFAULT-DEFINITION EXCLUDE-HERE-OBJECT?
	<DEFINE EXCLUDE-HERE-OBJECT? () <>>>

<DEFINE FIND-OBJECTS ("OPT" (SEARCH:FIX
			     <COND (<==? 1 <FIND-WHICH ,FINDER>>
				    <SYNTAX-SEARCH <PARSE-SYNTAX ,PARSE-RESULT>
						   1>)
				   (T
				    <SYNTAX-SEARCH <PARSE-SYNTAX ,PARSE-RESULT>
						   2>)>)
			    (PARENT:<OR OBJECT FALSE> <>)
			    (NO-ADJACENT <>)
		      "AUX" GLBS (CONT? T) N:FIX (RES <FIND-RES ,FINDER>))
  ;<MAKE-FIND-RES 'FIND-RES .RES 'FIND-RES-COUNT 0>
  <FIND-RES-COUNT .RES 0>
  <FIND-RES-NEXT .RES <>>
  ;"Initialize world"
  <COND (<AND .PARENT
	      ;<NOT <IN? .PARENT ,GLOBAL-OBJECTS>>
	      <OR <NOT <FIND-DESCENDANTS .PARENT
				<ORB ,FD-INCLUDE? ,FD-SEARCH? ,FD-NEST?>;7>>
		  <NOT <0? <FIND-RES-COUNT .RES>:FIX>>>>
	 ;"In case we have `the foo in the bar' or `a picture on the wall'"
	 ;<SET CONT? <>>
	 T)
	(T
	  <COND (<AND .PARENT <ZERO? .NO-ADJACENT>>
		 <IFFLAG (P-ARTHUR
			  <COND (<NOT <SET GLBS <FIND-ADJS ,FINDER>>>
				 <FIND-ADJS ,FINDER
					    <SET GLBS <PMEM-ALLOC ADJS POSS .PARENT>>>)
				(<NOT <ADJS-POSS .GLBS>>
				 <ADJS-POSS .GLBS .PARENT>)>)
			 (T
			  <COND (<NOT <SET GLBS <FIND-ADJS ,FINDER>>>
				 <FIND-ADJS ,FINDER
					    <SET GLBS <PMEM-ALLOC ADJS POSS .PARENT>>>)>
			  ;<COND (<NOT <ADJS-POSS .GLBS>>	;"must save old value?"
				  <ADJS-POSS .GLBS .PARENT>)>)>)>
	  <COND (<AND <T? <ANDB .SEARCH ,SEARCH-MOBY ;128>>
		      <F? <ANDB .SEARCH ,SEARCH-MUST-HAVE>>
		      <FIRST? ,GENERIC-OBJECTS>
		      ;<NOT <FIND-DESCENDANTS ,GENERIC-OBJECTS .SEARCH>>>
		 <REPEAT ((OBJ <FIRST? ,GENERIC-OBJECTS>))
			 <COND (<NOT <MATCH-OBJECT .OBJ ,FINDER T>>
				<RETURN>)
			       (<NOT <SET OBJ <NEXT? .OBJ>>>
				<RETURN>)>>
		 <COND (<NOT <0? <SET CONT? <FIND-RES-COUNT .RES>>:FIX>>
			<RETURN <1? .CONT?:FIX>>)>)>
	 <PROG ((LOSING? <>))
	   <COND
	    (<OR <AND <NOT .LOSING?>	;"redundant?"
		      <NOT <0? <ANDB .SEARCH ,SEARCH-CARRIED ;12>>>>
		 .LOSING?>
	     <SET CONT?
		  <FIND-DESCENDANTS ,WINNER
		   <FD-FLAG FD-NOTOP?
		    <FD-FLAG FD-INCLUDE?
		     <FD-FLAG FD-NEST? ,FD-SEARCH?
		      <OR .LOSING? ;"search pockets?"
			  <NOT <0? <ANDB .SEARCH ,SEARCH-POCKETS ;8>>>>>
		     <OR .LOSING?
			 <NOT <0? <ANDB .SEARCH ,SEARCH-CARRIED ;12>>>>>
		    <AND <NOT .LOSING?>
			 <0? <ANDB .SEARCH ,SEARCH-HELD ;4>>>>>>)>
	   <COND
	    (<OR .LOSING?
		 <NOT <0? <ANDB .SEARCH ,SEARCH-IN-ROOM ;3>>>>
	     <SET CONT?
		  <FIND-DESCENDANTS ,HERE
		   <FD-FLAG FD-NOTOP?
		    <FD-FLAG FD-NEST?
		     <FD-FLAG FD-INCLUDE? ,FD-SEARCH?
		      <AND ;,LIT
			   <OR .LOSING?
			       <NOT <0? <ANDB .SEARCH ,SEARCH-IN-ROOM ;3>>>>>>
		     <OR .LOSING?
			 <NOT <0? <ANDB .SEARCH ,SEARCH-OFF-GROUND ;2>>>>>
		    <AND <NOT .LOSING?>
			 <0? <ANDB .SEARCH ,SEARCH-ON-GROUND ;1>>>>>>)>
	   <COND (<NOT <0? <FIND-RES-COUNT .RES>>>
		  <RETURN>)
		 (<AND <NOT <BTST .SEARCH ,SEARCH-ALL>>
		       <NOT .LOSING?>>
		  <COND (<AND <SET GLBS <LEXV-WORD ,TLEXV>>
			      <OR <T? <WORD-CLASSIFICATION-NUMBER .GLBS>>
				  <T? <WORD-SEMANTIC-STUFF .GLBS>>>>
			 <SET LOSING? T>	;"not a sample command"
			 <AGAIN>)
			(<AND <BAND ,SEARCH-MUST-HAVE .SEARCH>
			      <NOT <BAND ,SEARCH-MOBY .SEARCH>>>
			 <RFALSE>)>)>
	   <COND (<SET GLBS <GETPT ,HERE ,P?GLOBAL>>
		  <COND (T ;<CHECK-EXTENDED?>
			   <SET N </ <PTSIZE .GLBS>:FIX 2>>)
			;(T <SET N <- <PTSIZE .GLBS>:FIX 1>>)>
		  <REPEAT (O:OBJECT)
		    <COND (<L? <SET N <- .N 1>> 0>
			   <RETURN>)
			  (<NOT <SET CONT?
				     <MATCH-OBJECT
				      <COND (T ;<CHECK-EXTENDED?>
						<SET O <ZGET .GLBS .N>>)
					    ;(T  <SET O <GETB .GLBS .N>>)>
				      ,FINDER T>>>
			   <RETURN>)
			  (<AND <FIRST? .O>
				<NOT <0? <ANDB .SEARCH ,SEARCH-OFF-GROUND>>>
				<SEARCH-IN-LG? .O>>
			   <COND
			    (<NOT
			      <SET CONT?
				   <FIND-DESCENDANTS .O ,FD-INCLUDE? ;1>>>
			     <RETURN>)>)>>)>
	   <COND (<AND .CONT?
		       <NOT <EXCLUDE-HERE-OBJECT?>>>
		  <SET CONT? <MATCH-OBJECT ,HERE ,FINDER T>>)>
	   <COND (<AND .CONT?
		       <NOT <EQUAL? ,HERE <LOC ,PLAYER>>>
		       <GETP <LOC ,PLAYER> ,P?THINGS>>
		  <SET CONT? <ZAPPLY ,TEST-THINGS <LOC ,PLAYER> ,FINDER>>)>
	   <COND (<AND .CONT?
		       <GETP ,HERE ,P?THINGS>>
		  <SET CONT? <ZAPPLY ,TEST-THINGS ,HERE ,FINDER>>)>
	   <COND (<NOT <0? <FIND-RES-COUNT .RES>>>
		  <SET CONT? <>>)>
	   <COND (.CONT?
		  <SET CONT?
		       <FIND-DESCENDANTS ,GLOBAL-OBJECTS
			<FD-FLAG FD-NEST? ,FD-INCLUDE?
			 <NOT <0? <ANDB .SEARCH ,SEARCH-OFF-GROUND ;2>>>>
			;<COND (<BTST .SEARCH 2> 5) (T 1)>>>)>
	   <COND (<AND .CONT?
		       <0? <FIND-RES-COUNT .RES>:FIX>
		       ;<BTST .SEARCH ,SEARCH-ADJACENT>
		       <NOT .NO-ADJACENT>
		       <SET GLBS <GETP ,HERE ,P?ADJACENT>>>
		  <SET N <GETB .GLBS 0>>
		  <REPEAT (;(SCH <ANDB .SEARCH <XORB -1 ,SEARCH-ADJACENT>>))
		    <COND (<T? <GETB .GLBS .N>>	;"room visible now?"
			   <SET N <- .N 1>>
			   <FIND-OBJECTS ,SEARCH-ON-GROUND <GETB .GLBS .N> T>)
			  (T
			   <SET N <- .N 1>>)>
		    <COND (<L? <SET N <- .N 1>> 1>
			   <RETURN>)>>
		  <COND (<NOT <0? <FIND-RES-COUNT .RES>:FIX>>
			 <SET CONT? <>>)>)>
	   <COND
	    (<AND .CONT?
		  <0? <FIND-RES-COUNT .RES>:FIX>
		  <MOBY-FIND? .SEARCH>>
	     <REPEAT ((OBJ 1))
	         <COND (T ;<AND <NOT <FSET? .OBJ ,INVISIBLE>>
			     ;<NOT <IN? .OBJ ,ROOMS>>>
			<COND (<NOT <MATCH-OBJECT .OBJ ,FINDER T>>
			       <RETURN>)>)>
		 <COND (<G? <SET OBJ <+ .OBJ 1>> ,LAST-OBJECT>
			<RETURN>)>>)>>)>
  ;<COND (<AND <L? 1 <FIND-RES-COUNT .RES>:FIX>
	      <FIND-OF ,FINDER>>
	 <MATCH-OF-OBJECTS .RES>)>
  <1? <FIND-RES-COUNT .RES>:FIX>>

"old default pseudo stuff, which doesn't work if you use it"
;<DEFAULT-DEFINITION PSEUDO-OBJECTS
<DEFINE20 PSEUDO ("TUPLE" V)
	<MAPF ,PLTABLE
	      <FUNCTION (OBJ)
		   <COND (<N==? <LENGTH .OBJ> 3>
			  <ERROR BAD-THING .OBJ PSEUDO>)>
		   <MAPRET <COND (<NTH .OBJ 2>
				  <VOC <SPNAME <NTH .OBJ 2>> NOUN>)>
			   <COND (<NTH .OBJ 1>
				  <VOC <SPNAME <NTH .OBJ 1>> ADJ>)>
			   <NTH .OBJ 3>>>
	      .V>>

<GLOBAL LAST-PSEUDO-LOC:OBJECT <>>

<COND (<CHECK-VERSION? ZIP>
       <OBJECT PSEUDO-OBJECT
		(LOC LOCAL-GLOBALS)
		(DESC "pseudo")
		(ACTION 0)>)
      (T
       <OBJECT PSEUDO-OBJECT
		(LOC LOCAL-GLOBALS)
		(DESC "pseudoxxx")
		(ACTION 0)	;"no other properties!">)>

<DEFINE TEST-THINGS (RM F "AUX" CT)
	<COND (<T? <SET CT <FIND-ADJS .F>>>
	       <SET CT <ADJS-COUNT .CT>>)>
	<REPEAT ((NOUN <FIND-NOUN .F>)
		 (V <REST-TO-SLOT <FIND-ADJS .F> ADJS-COUNT 1>)
		 (GLBS <GETP .RM ,P?THINGS>)
		 (N <ZGET .GLBS 0>))
		<SET N <- .N 3>>
		<COND (<L? .N 0>
		       <RTRUE>)
		      (<AND <EQUAL? .NOUN ;<> ,W?ONE <ZGET .GLBS <+ .N 1>>>
			    <OR <0? .CT>
				<ZMEMQ <ZGET .GLBS <+ .N 2>> .V .CT>>>
		       <SETG LAST-PSEUDO-LOC .RM>
		       <PUTP ,PSEUDO-OBJECT
			     ,P?ACTION
			     <ZGET .GLBS <+ .N 3>>>
		       <SET V <ZBACK <GETPT ,PSEUDO-OBJECT ,P?ACTION> 7>>
		       <COND (T ;<CHECK-EXTENDED?>
			      <COPYT .NOUN .V 6>)
			     ;(T
			      <ZPUT .V 0 <ZGET .NOUN 0>>
			      <ZPUT .V 1 <ZGET .NOUN 1>>)>
		       <ADD-OBJECT ,PSEUDO-OBJECT .F>
		       <RFALSE>)>>>>

"new default pseudo stuff, which probably doesn't work either"
<DEFAULT-DEFINITION PSEUDO-OBJECTS
<PUTPROP THINGS PROPSPEC HACK-PSEUDOS>

<DEFINE20 HACK-PSEUDOS (LIST "AUX" (N 0) (CT 0))
  <SET LIST <REST .LIST>>
  <SET LIST
    <MAPR ,LIST
      <FUNCTION (X "AUX" L (ACT 0) (NCT 0))
        <COND (<0? .N>
	       <SET CT <+ .CT 1>>
	       <SET N 1>
	       <COND (<TYPE? <1 .X> ATOM>
		      <SET ACT 1>
		      ;<TABLE (PURE)
			     1
			     <VOC <SPNAME <1 .X>> ADJ>>)
		     (<TYPE? <1 .X> LIST>
		      <SET ACT <LENGTH <1 .X>>>
		      ;<EVAL <CHTYPE (TABLE (PURE)
				     <LENGTH <1 .X>>
				     !<MAPF ,LIST
				       <FUNCTION (Y)
				         <VOC <SPNAME .Y> ADJ>>
				       <1 .X>>) FORM>>)
		     (T
		      <SET ACT 0>)>
	       <COND (<LENGTH? .X 1>)
		     (<TYPE? <2 .X> ATOM>
		      <SET NCT 1>)
		     (<TYPE? <2 .X> LIST>
		      <SET NCT <LENGTH <2 .X>>>)
		     (T
		      <SET NCT 0>)>
	       <TABLE (PURE)
		      <BYTE .ACT> <BYTE .NCT>
		      <COND (<0? .ACT>
			     0)
			    (<==? .ACT 1>
			     <VOC <SPNAME <1 .X>> ADJ>)
			    (T
			     <EVAL <CHTYPE (TABLE (PURE)
					    ;.ACT
					    !<MAPF ,LIST
						<FUNCTION (Y)
						 <VOC <SPNAME .Y> ADJ>>
						<1 .X>>) FORM>>)>
		      <COND (<0? .NCT>
			     0)
			    (<==? .NCT 1>
			     <VOC <SPNAME <2 .X>> NOUN>)
			    (T
			     <EVAL <CHTYPE (TABLE (PURE)
					    ;.NCT
					    !<MAPF ,LIST
						<FUNCTION (Y)
						 <VOC <SPNAME .Y> NOUN>>
						<2 .X>>) FORM>>)>>)
	      (<1? .N>
	       <SET N 2>
	       <MAPRET>)
	      (T
	       <SET N 0>
	       <1 .X>)>>
      .LIST>>
  (<> <EVAL <CHTYPE (TABLE (PURE ;PATTERN ;(BYTE [REST WORD]))
		     .CT !.LIST) FORM>>)>

<DEFINE TEST-THINGS (RM F
		     "AUX" CT (RMG <GETP .RM ,P?THINGS>) (RMGL <GET .RMG 0>))
 <SET RMG <REST .RMG 2>>
 <COND (<T? <SET CT <FIND-ADJS .F>>>
	<SET CT <ADJS-COUNT .CT>>)>
 <REPEAT (TTBL (NOUN <FIND-NOUN .F>) XCT
	       (V <REST-TO-SLOT <FIND-ADJS .F> ADJS-COUNT 1>))
  <SET TTBL <GET .RMG 0 ;1>>
  <COND (<AND <OR <EQUAL? .NOUN ;<> ,W?ONE>
		  <AND <1? <SET XCT <GETB .TTBL 1>>>
		       <EQUAL? .NOUN <ZGET .TTBL 2>>>
		  <INTBL? .NOUN <ZGET .TTBL 2> .XCT>>
	      <OR <0? .CT>
		  <AND <1? <SET XCT <GETB .TTBL 0>>>
		       <EQUAL? <ZGET .V 0> <ZGET .TTBL 1>>>
		  <INTBL? <ZGET .V 0> <ZGET .TTBL 1> .XCT>>
	      <OR <NOT <FIND-OF .F>>
		  <AND <EQUAL? 1 <FIND-RES-COUNT ,OWNER-SR-HERE>>
		       <EQUAL? ,PSEUDO-OBJECT <FIND-RES-OBJ1 ,OWNER-SR-HERE>>
		       <EQUAL? ,LAST-PSEUDO-LOC .RM>
		       <EQUAL? <GETP ,PSEUDO-OBJECT ,P?ACTION> <GET .RMG 1>>>>>
	 <SETG LAST-PSEUDO-LOC .RM>
	 <PUTP ,PSEUDO-OBJECT ,P?ACTION <GET .RMG 1 ;2>>
	 <SET V <ZBACK <GETPT ,PSEUDO-OBJECT ,P?ACTION> 7>>
	 <COPYT .NOUN .V 6>
	 <COND (<BTST <WORD-FLAGS .NOUN> ,PLURAL-FLAG>
		<FSET ,PSEUDO-OBJECT ,PLURAL>)
	       (T
		<FCLEAR ,PSEUDO-OBJECT ,PLURAL>)>
	 <ADD-OBJECT ,PSEUDO-OBJECT .F>
	 <RFALSE>)>
  <SET RMG <ZREST .RMG 4 ;6>>
  <COND (<L? <SET RMGL <- .RMGL 1>> 1>
	 <RTRUE>)>>>

<GLOBAL LAST-PSEUDO-LOC:OBJECT <>>

<OBJECT PSEUDO-OBJECT
	(LOC LOCAL-GLOBALS)
	(DESC "pseudoxxx")
	(ACTION 0)	;"no other properties!">>

<END-SEGMENT>
<END-DEFINITIONS>
