! *********************************************************************
!* ===================================================================
!*
!* DATE                         : Oct. 2010
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: NEWUNIT= specifier, Feature#:377344
!* SECONDARY FUNTIONS TESTED    : READ,WRITE,REWIND
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : NEWUNIT with rec,read,write,wait,
!*                                FORM='UNFORMATTED'
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/10/21    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    PROGRAM FXOPEN_NEWUNIT10
    IMPLICIT NONE

    INTEGER i /0/,j /0/,IVAR
    INTEGER Iarray(5,5)
    INTEGER ID_Number(5) /5*-1/

    OPEN (NEWUNIT=IVAR, FORM='UNFORMATTED', ACCESS='DIRECT',  &
  & ACTION='READWRITE',STATUS='SCRATCH', ASYNCH='YES', RECL=8)

    DO i = 1,5
     DO j = 1, 5
        Iarray(i,j)=i*10+j
        WRITE (IVAR, REC=(i-1)*5+j) Iarray(i,j)
     END DO
    END DO

    READ (IVAR, REC=1, ID=ID_Number(1)) Iarray(1,1)
    READ (IVAR, REC=2, ID=ID_Number(2)) Iarray(1,2)

    DO i = 1,2
     WAIT (ID=ID_Number(i))
    END DO

    PRINT *,Iarray(1,1)
    PRINT *,Iarray(1,2)

    WRITE (IVAR, REC=1, ID=ID_Number(1)) Iarray(1,1)
    WRITE (IVAR, REC=2, ID=ID_Number(2)) Iarray(1,2)

    DO i = 1,2
     WAIT (ID=ID_Number(i))
    END DO

    PRINT *,Iarray(1,1)
    PRINT *,Iarray(1,2)

    CLOSE(IVAR)

   END PROGRAM FXOPEN_NEWUNIT10
