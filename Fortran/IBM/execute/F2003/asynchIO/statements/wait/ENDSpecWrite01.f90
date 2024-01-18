!*  ===================================================================
!*
!*  DATE                       : March  8, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() on Pending Write Data Transfer
!*                               Operations (with/without ID= Specifier)
!*                               for a Specific Unit
!*  SECONDARY FUNCTIONS TESTED : END= Specifier
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), END= Specifier, ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.6.1 WAIT statement
!*
!*  A WAIT statement performs a wait operation for specified pending
!*  asynchronous data transfer operations.
!*
!*  R921 wait-stmt  is  WAIT (wait-spec-list)
!*  R922 wait-spec  is  [ UNIT = ] file-unit-number
!*                  or  END = label
!*                  or  EOR = label
!*                  or  ERR = label
!*                  or  ID = scalar-int-expr
!*                  or  IOMSG = iomsg-variable
!*                  or  IOSTAT = scalar-int-variable
!*
!*  ...
!*
!*  NOTE 9.52
!*  ...
!*  And END= specifier has no effect if the pending data transfer operation
!*  is not a READ.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM ENDSpecWrite01
    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j

    INTEGER :: oStat
    INTEGER, DIMENSION( 10 ) :: ids

    CHARACTER(LEN = 256) :: oMsg


    OPEN(IOMSG=oMsg, IOSTAT=oStat, ACTION='write', RECL=15,&
        &FORM='formatted', ASYNCHRONOUS='yes', ACCESS='direct', UNIT=111)
    IF (oStat <> 0) THEN
        WRITE(0, *) "OPEN() <", oStat, "> ", oMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 10
        WRITE(FMT='(5I3)', IOSTAT=oStat, UNIT=111, REC=(i * 3), ID=ids( i ),&
            &ASYNCHRONOUS='yes', IOMSG=oMsg) (/ ((j + i), j = 55, 145, 10) /)
        IF (oStat <> 0) THEN
            WRITE(0, *) "WRITE() <", oStat, "> ", oMsg
            CALL zzrc( (10 + i) )
        END IF
    END DO


    DO j = 10, 1, 3
        WAIT(END=100, ID=ids( j ), UNIT=111, IOSTAT=oStat)
    END DO


    WAIT(111, END=200, IOMSG=oMsg)


    CLOSE(UNIT=111, IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", oStat, "> ", oMsg
        CALL zzrc( 41 )
    END IF


    GOTO 300


100 CONTINUE
    WRITE(0, *) "WAIT(ids(", j, ")) <", oStat, ">"
    CALL zzrc( (20 + j) )

200 CONTINUE
    WRITE(0, *) "WAIT() ", oMsg
    CALL zzrc( 31 )

300 CONTINUE

END PROGRAM ENDSpecWrite01
