;"SORT-MACROS by JM"

<PACKAGE "sort-macros">

<ENTRY MERGESORT-MACRO>

<DEFMAC MERGESORT-MACRO ('OUTER 'L 'LEN 'L=?)
    <FORM BIND ((LEN .LEN))
        <FORM COND (<FORM L=? '.LEN 1> .L)
                   (T
                    <FORM BIND ((1ST-HALF .L)
                                (LEN/2 <FORM / '.LEN 2>)
                                (1ST-HALF-END <FORM REST '.1ST-HALF <FORM - '.LEN/2 1>>)
                                (2ND-HALF <FORM REST '.1ST-HALF-END>)
                                (RESULT '(<>))
                                (RESULT-END '.RESULT))
                        <FORM PUTREST '.1ST-HALF-END '()>
                        <FORM SET 1ST-HALF <FORM .OUTER '.1ST-HALF '.LEN/2>>
                        <FORM SET 2ND-HALF <FORM .OUTER '.2ND-HALF <FORM - '.LEN '.LEN/2>>>
                        <FORM REPEAT '(V)
                            <FORM COND (<FORM EMPTY? '.1ST-HALF>
                                        <FORM PUTREST '.RESULT-END '.2ND-HALF>
                                        <FORM RETURN>)
                                       (<FORM EMPTY? '.2ND-HALF>
                                        <FORM PUTREST '.RESULT-END '.1ST-HALF>
                                        <FORM RETURN>)
                                       (<FORM .L=? <FORM 1 '.1ST-HALF> <FORM 1 '.2ND-HALF>>
                                        <FORM SET V <FORM 1 '.1ST-HALF>>
                                        <FORM SET 1ST-HALF <FORM REST '.1ST-HALF>>)
                                       (T
                                        <FORM SET V <FORM 1 '.2ND-HALF>>
                                        <FORM SET 2ND-HALF <FORM REST '.2ND-HALF>>)>
                            <FORM PUTREST '.RESULT-END <FORM SET RESULT-END ('.V)>>>
                        <FORM REST '.RESULT>>)>>>

<ENDPACKAGE>