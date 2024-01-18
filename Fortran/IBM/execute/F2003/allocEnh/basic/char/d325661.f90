!*  ===================================================================
!*
!*                               Type
!*
!*  DATE                       : September 21, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor with Implied-DO
!*  SECONDARY FUNCTIONS TESTED : The ac-value is a FUNCTION with an ALLOCATABLE
!*                               CHARACTER Result Value
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  The Reduced Code below uses the FUNCTION "CharFn()" with an ALLOCATABLE
!*  CHARACTER Return Value as the ac-value of the Implied-DO of an Array
!*  Constructor.  "CharFn()" is only called once instead of once per
!*  iteration of the Implied-DO.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM assumedDummyArg02

    CHARACTER(27) :: charArrayVar( 3 )

    charArrayVar = (/ (CharFn( i ), i = 1, 3) /)

    PRINT '("(",A27,")")', charArrayVar

    CONTAINS

        FUNCTION CharFn( n )
            INTEGER :: n

            CHARACTER(27), ALLOCATABLE :: CharFn

            ALLOCATE( CharFn )
            CharFn = REPEAT('zYxWuVtSr', n)

            PRINT *, "CharFn()", n, "'", CharFn, "'"

        END FUNCTION CharFn

END PROGRAM assumedDummyArg02
