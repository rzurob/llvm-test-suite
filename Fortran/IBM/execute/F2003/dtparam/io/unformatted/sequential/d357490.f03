!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : October 15, 2008
!*
!*  ABSTRACT                   : DTPARAM: ASTI: ICE: IO: Unformatted WRITE
!*                               of First Element of Array Dummy Argument
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  ORIGINAL TEST CASE         :
!*  F2003/dtparam/io/unformatted/sequential/sequentialReadWrite05.scenario
!*
!*  DESCRIPTION                :
!*  ASTI ICEs when the Reduced Code (below) is compiled.  This code
!*  performs an Unformatted WRITE of the first element from a dummy
!*  argument which is an array of derived type.
!*
!*  NOTE:  Defects:  d353875 and d355371 experience a similar ICE,
!*         however both of these Defects reference the substring of
!*         a CHARACTER Component.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

PROGRAM d357490
    IMPLICIT NONE

    TYPE tR(l1)
        INTEGER, LEN :: l1
        INTEGER :: i = -1
    END TYPE tR

    TYPE(tR(1)) :: bR( 1 ) = [ tR(1)() ]

    OPEN(99, ACTION='write', FORM='unformatted')
    CALL WriteTR8( bR )

    CONTAINS

        SUBROUTINE WriteTR8( dTR )
            TYPE(tr(*)) :: dTR( : )

            WRITE( 99 ) dTR( 1 )
        END SUBROUTINE WriteTR8

END PROGRAM d357490
