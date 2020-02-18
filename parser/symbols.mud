<DEFINITIONS "SYMBOLS">

<USE "NEWSTRUC">

<INCLUDE "SET">

<BLOCK (<ROOT>)>
GET-CLASSIFICATION
NUMBER-WORD-CLASSES
<ENDBLOCK>

<SET-DEFSTRUCT-FILE-DEFAULTS>

;"SYMBOLS"			     

<DEFSTRUCT SYMBOL
	   (VECTOR ('PRINTTYPE PRINT-SYMBOL))
	   (SYM-NAME ATOM 'NONE)
	   (SYM-NUMBER <OR FALSE FIX> <>)
	   (SYM-PRODS LIST ())
	   (SYM-FIRSTS SET <MAKE-SET>)
	   (SYM-WEIGHT FIX '1)>

<DEFINE PRINT-SYMBOL (SYM:SYMBOL)
  <PRIN1 <SYM-NAME .SYM>>>

<GDECL (NUM-OF-TERMINALS NUM-OF-SYMBOLS NUM-OF-NONTERMINALS) FIX
       (ALL-SYMBOLS) <LIST [REST SYMBOL]>
       (EPSILON-SYMBOL START-SYMBOL END-OF-INPUT-SYMBOL) SYMBOL
       (SYMBOL-VECTOR) <VECTOR [2 VECTOR]>>

<GDECL (ALL-TERMINALS ALL-NONTERMINALS) <LIST [REST SYMBOL]>>

<COND (<NOT <GASSIGNED? ALL-SYMBOLS>>
       <SETG ALL-SYMBOLS ()>)>

<DEFINE RESET-SYMBOLS ("OPT" (TERMINALS? <>))
  <MAPF %<>
	<FUNCTION (SYM:SYMBOL)
	  <COND (<OR .TERMINALS?
		     <NOT <MEMQ .SYM ,ALL-TERMINALS>>>
		 <PUTPROP <SYM-NAME .SYM> SYMBOL>)>>
	,ALL-SYMBOLS>
  <COND (.TERMINALS? <SETG ALL-TERMINALS ()>)>
  <SETG ALL-NONTERMINALS ()>
  <SETG ALL-SYMBOLS <LIST !,ALL-TERMINALS>>
  <SETG ALL-PRODUCTIONS ()>
  <COND (.TERMINALS? <SETG NUM-OF-TERMINALS 0>)>
  <SETG NUM-OF-NONTERMINALS 0>
  <SETG NUM-OF-SYMBOLS ,NUM-OF-TERMINALS>
  <SETG NUM-OF-PRODUCTIONS 0>
  <SETG EPSILON-SYMBOL <MAKE-INTERNAL EPSILON>>
  <SETG START-SYMBOL <MAKE-INTERNAL START>>>

<DEFINE TERMINALS T ("TUPLE" ATMS:<TUPLE [REST <OR ATOM <LIST ATOM FIX>>]>)
  <COND (<G? <LENGTH .ATMS> 14>
	 <PRINT-MANY .OUTCHAN PRINC <LENGTH .ATMS> " terminals defined."
		     PRMANY-CRLF>)>
  <SETG NUMBER-WORD-CLASSES <+ <LENGTH .ATMS> 1>>
  <RESET-SYMBOLS T>
  <MAPF %<> ,MAKE-TERMINAL .ATMS>
  <SETG END-OF-INPUT-SYMBOL <MAKE-TERMINAL END-OF-INPUT>>>

<DEFINE MAKE-INTERNAL (ATM:ATOM "VALUE" SYMBOL)
  <BIND ((SYM:SYMBOL <MAKE-SYMBOL .ATM>))
    <PUTPROP .ATM SYMBOL .SYM>
    .SYM>>

<SETG SYMBOL-OBS <MOBLIST SYMBOLS>>

<DEFMAC GET-CLASSIFICATION-NUMBER ('ATM)
  <COND (<TYPE? .ATM ATOM>
	 <SET ATM <SPNAME .ATM>>)>
  <COND (<TYPE? .ATM STRING>
	 <CHTYPE <OR <LOOKUP .ATM ,SYMBOL-OBS>
		     <INSERT .ATM ,SYMBOL-OBS>>
		 GVAL>)
	(T
	 <FORM GET-CLASSIFICATION .ATM>)>>

<DEFINE BUILD-SYMBOL (ATM:ATOM ID:FIX "AUX" (NAM <SPNAME .ATM>)"VALUE" SYMBOL)
  <BIND ((SYM:SYMBOL <MAKE-INTERNAL .ATM>))
    <PUTPROP .ATM SYMBOL .SYM>
    <SYM-NUMBER .SYM .ID>
    <SETG <OR <LOOKUP .NAM ,SYMBOL-OBS>
	      <INSERT .NAM ,SYMBOL-OBS>>
	  .ID>
    <SETG ALL-SYMBOLS (.SYM !,ALL-SYMBOLS)>
    .SYM>>

<MSETG MAX-TERMINALS 30>

<DEFINE MAKE-TERMINAL (FROB:<OR ATOM <LIST ATOM FIX>>
		       "AUX" ATM:ATOM (WEIGHT:FIX 1)
			     ID TERM:SYMBOL "VALUE" SYMBOL)
  <COND (<G? <SETG NUM-OF-TERMINALS <+ ,NUM-OF-TERMINALS 1>> ,MAX-TERMINALS>
	 <ERROR TOO-MANY-TERMINALS!-ERRORS .FROB MAKE-TERMINAL>)
	(T
	 <COND (<TYPE? .FROB LIST>
		<SET ATM <1 .FROB>>
		<SET WEIGHT <2 .FROB>>)
	       (T
		<SET ATM .FROB>)>
	 <COND (<G? <SET ID ,NUM-OF-TERMINALS> 15>
		<SET ID <ORB *100000* <LSH 1 <- .ID 16>>>>)
	       (T
		<SET ID <LSH 1 <- .ID 1>>>)>
	 <SET TERM <BUILD-SYMBOL .ATM .ID>>
	 <SYM-WEIGHT .TERM .WEIGHT>
	 <SETG ALL-TERMINALS (.TERM !,ALL-TERMINALS)>
	 .TERM)>>

<DEFINE MAKE-NONTERMINAL (ATM:ATOM "AUX" NONTERM:SYMBOL "VALUE" SYMBOL)
  <SET NONTERM <BUILD-SYMBOL .ATM <- <SETG NUM-OF-NONTERMINALS
					   <+ ,NUM-OF-NONTERMINALS 1>>>>>
  <SETG ALL-NONTERMINALS (.NONTERM !,ALL-NONTERMINALS)>
  .NONTERM>

<DEFINE LOOKUP-SYMBOL (ATM:ATOM "VALUE" SYMBOL)
  <OR <GETPROP .ATM SYMBOL> <MAKE-NONTERMINAL .ATM>>>

<END-DEFINITIONS>