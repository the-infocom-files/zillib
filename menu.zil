"MENU for
				Library
	(c) Copyright 1988 Infocom, Inc. All Rights Reserved."

"Use this by calling GET-FROM-MENU. It takes a prompt message (a string),
a menu (an LTABLE of byte LTABLEs of menu items), a handler (a routine),
and the offset of the initial menu item to be highlighted (a number, by
default 1).  When an item is selected, the handler is passed the number of
the item picked, and the menu itself. If the handler returns non-false, it
means to leave GET-FROM-MENU, returning the non-false value. If the handler
returns false, it means to fall back into GET-FROM-MENU."

<BEGIN-SEGMENT 0>

<DEFAULTS-DEFINED MENU-WINDOW	;"what window to use"
		  DIROUT-TABLE	;"used to measure sizes of strings">

<DEFAULT-DEFINITION MENU-WINDOW
	<CONSTANT MENU-WINDOW 2>>

;<DEFAULT-DEFINITION DIROUT-TABLE
	<CONSTANT DIROUT-TABLE <ITABLE 80 <BYTE 0>>>>

<ROUTINE GET-FROM-MENU (MSG MENU FCN "OPT" (DEF 1)
			"AUX" YFULL Y TMP N WHICH (L <GET .MENU 0>) (WID 0))
	 <COND (<OR <APPLE?> <EQUAL? ,MACHINE ,AMIGA>>
		<SET L <+ .L 1>>)>
	 <COND (<SET Y <WINGET ,S-TEXT ,WCRCNT>>
		<N-CRLF .Y>
		<RESET-MARGIN>)>
	 <SET TMP </ <WINGET 0 ,WHIGH> ,FONT-Y>>
	 <COND (<G? .L <- .TMP <WINGET 0 ,WLCNT>>>
		<WINPUT 0 ,WLCNT .TMP>)>
	 <REPEAT ((CNT 0) (LL <GET .MENU 0>))
		 <COND (<IGRTR? CNT .LL>
			<SET WID <+ .WID 1>>
			<RETURN>)
		       (ELSE
			<DIROUT ,D-TABLE-ON ,DIROUT-TABLE>
			<PRINT-TABLE <GET .MENU .CNT>>
			<DIROUT ,D-TABLE-OFF>
			<SET TMP <LOWCORE TWID>>
			<COND (<G? .TMP .WID> <SET WID .TMP>)>)>>
	 <SETG RESTORED? T>
	 <REPEAT (TMP)
		 <COND (,RESTORED?
			<MAKE-ROOM-FOR <* .L ,FONT-Y>>
			<SET Y <WINGET 0 ,WYPOS>>
			<SET YFULL <+ <WINGET 0 ,WTOP> .Y -1>>)>
		 <CURSET .Y 1>
		 <WINDEF ,MENU-WINDOW
			 .YFULL
			 <WINGET 0 ,WLEFT>
			 <* .L ,FONT-Y>
			 <WINGET 0 ,WWIDE>>
		 <SCREEN ,MENU-WINDOW>
		 <COND (<NOT <EQUAL? ,MACHINE ,AMIGA>>
			<SET TMP <WINGET ,S-TEXT ,WCOLOR>>
			<COLOR <LOW-BYTE .TMP> <HIGH-BYTE .TMP>>)>
		 <CLEAR ,MENU-WINDOW>
		 <SCREEN ,S-TEXT>
		 <TELL .MSG>
		 <COND (<OR <APPLE?> <EQUAL? ,MACHINE ,AMIGA>>
			<SET TMP ,FONT-Y>
			<CRLF>)
		       (ELSE <SET TMP 0>)>
		 <SET WHICH
		      <MENU-SELECT .MENU
				   <+ .YFULL .TMP>
				   .WID
				   .DEF>>
		 <COND (<L? .WHICH 0> <RETURN .WHICH>)
		       (<FLAG-ON? ,F-SCRIPT>
			<DIROUT ,D-SCREEN-OFF>
			<TELL " ">
			<PRINT-TABLE <GET .MENU .WHICH>>
			<CRLF>
			<CRLF>
			<DIROUT ,D-SCREEN-ON>)>
		 <CRLF>
		 <SETG RESTORED? <>>
		 <COND (<SET TMP <APPLY .FCN .WHICH .MENU>>
			<RETURN .TMP>)>>>

<ROUTINE MENU-SELECT (M Y WID "OPT" (S 1)
		      "AUX" X (CNT <GET .M 0>) CHR)
	 <SET X </ <- <LOWCORE HWRD> .WID> 2>>
	 <WINDEF ,MENU-WINDOW .Y .X <* .CNT ,FONT-Y> .WID>
	 <SCREEN ,MENU-WINDOW>
	 <COND (<NOT <EQUAL? ,MACHINE ,AMIGA>>
		<SET CHR <WINGET ,S-TEXT ,WCOLOR>>
		<COLOR <LOW-BYTE .CHR> <HIGH-BYTE .CHR>>)>
	 <CLEAR ,MENU-WINDOW>
	 <CURSOR-OFF>
	 <REPEAT ((Y 0))
		 <COND (<G? <SET Y <+ .Y 1>> .CNT> <RETURN>)
		       (ELSE
			<CCURSET .Y 1>
			<COND (<EQUAL? .Y .S>
			       <HLIGHT ,H-INVERSE>)>
			<PRINT-TABLE <GET .M .Y>>
			<COND (<EQUAL? .Y .S>
			       <HLIGHT ,H-NORMAL>)>)>>
	 <CCURSET .S 1>
	 <REPEAT ((ICNT 0) OS OICNT (ITEM <GET .M .S>) TMP)
		 <SET CHR <LC <CONVERT-KEYS <READ-CHAR> ;<INPUT 1>>>>
		 <SET OS .S>
		 <SET OICNT .ICNT>
		 <COND (<AND <EQUAL? .CHR ,CLICK1 ,CLICK2>
			     <SET TMP <IN-WINDOW? ,SOFT-WINDOW>>>
			<SET S .TMP>
			<COND (<NOT <EQUAL? .OS .S>>
			       <CCURSET .OS 1>
			       <PRINT-TABLE <GET .M .OS>>
			       <CCURSET .S 1>
			       <HLIGHT ,H-INVERSE>
			       <SET ITEM <GET .M .S>>
			       <PRINT-TABLE .ITEM>
			       <HLIGHT ,H-NORMAL>
			       <CURSET <L-PIXELS .S>
				       <+ <STRWIDTH .ITEM .ICNT> 1>
				        ;<+ .ICNT 1>>)>
			<COND (<EQUAL? .CHR ,CLICK2>
			       <CLEAR ,MENU-WINDOW>
			       <CURSOR-ON>
			       <SCREEN ,S-TEXT>
			       <RETURN .S>)
			      (ELSE
			       <SET OS .S>)>)
		       (<AND <EQUAL? .CHR 8 127> <G? .ICNT 0>>
			<SET ICNT <- .ICNT 1>>)
		       (<EQUAL? .CHR ,UP-ARROW ,LEFT-ARROW
				,DOWN-ARROW ,RIGHT-ARROW 32>
			<SET ITEM <>>
			<SET ICNT <>>
			<COND (<EQUAL? .CHR ,UP-ARROW ,LEFT-ARROW>
			       <COND (<G? .S 1>
				      <SET S <- .S 1>>)
				     (ELSE
				      <SET S .CNT>)>)
			      (ELSE
			       <COND (<L? .S .CNT>
				      <SET S <+ .S 1>>)
				     (ELSE
				      <SET S 1>)>)>)
		       (<EQUAL? .CHR 13>
			<CLEAR ,MENU-WINDOW>
			<CURSOR-ON>
			<SCREEN ,S-TEXT>
			<RETURN .S>)
		       (<AND <L? .ICNT <GETB .ITEM 0>>
			     <EQUAL? .CHR <LC <GETB .ITEM <+ .ICNT 1>>>>>
			<SET ICNT <+ .ICNT 1>>)
		       (<SET TMP <FIND-ITEM .ITEM .CHR .ICNT .M>>
			<SET S .TMP>
			<SET ICNT <+ .ICNT 1>>)
		       (ELSE
			<SOUND ,S-BEEP>)>
		 <SET ITEM <GET .M .S>>
		 <COND (<OR <NOT <EQUAL? .S .OS>>
			    <NOT <EQUAL? .ICNT .OICNT>>>
			<COND (<NOT <EQUAL? .S .OS>>
			       <CCURSET .OS 1>
			       <PRINT-TABLE <GET .M .OS>>
			       <CCURSET .S 1>
			       <HLIGHT ,H-INVERSE>
			       <PRINT-TABLE .ITEM>
			       <HLIGHT ,H-NORMAL>)>
			<CURSET <L-PIXELS .S>
				<+ <STRWIDTH .ITEM .ICNT> 1>>)>>>

<ROUTINE PRINT-TABLE (TBL "OPT" (L <GETB .TBL 0>))
	 <PRINTT <REST .TBL> .L>>

<ROUTINE STRWIDTH (ITEM ICNT)
	 <COND (<G? .ICNT 1>
		<DIROUT ,D-TABLE-ON ,DIROUT-TABLE>
		<PRINT-TABLE .ITEM .ICNT>
		<DIROUT ,D-TABLE-OFF>
		<LOWCORE TWID>)
	       (ELSE 0)>>

<ROUTINE LC (CHR)
	 <COND (<AND <G=? .CHR !\A>
		     <L=? .CHR !\Z>>
		<+ .CHR 32>)
	       (ELSE .CHR)>>

<ROUTINE FIND-ITEM (ITEM CHR ICNT MENU)
	 <REPEAT ((S 0) NITEM)
		 <COND (<IGRTR? S <GET .MENU 0>> <RFALSE>)>
		 <SET NITEM <GET .MENU .S>> 
		 <COND (<AND <L? .ICNT <GETB .NITEM 0>>
			     <EQUAL? .CHR <LC <GETB .NITEM <+ .ICNT 1>>>>>
			<REPEAT ((CCNT 0) C1 C2)
				<COND (<IGRTR? CCNT .ICNT> <RETURN .S>)>
				<SET C1 <LC <GETB .NITEM .CCNT>>>
				<SET C2 <LC <GETB .ITEM .CCNT>>>
				<COND (<NOT <EQUAL? .C1 .C2>>
				       <RETURN>)>>)>>>

<END-SEGMENT ;"0">
