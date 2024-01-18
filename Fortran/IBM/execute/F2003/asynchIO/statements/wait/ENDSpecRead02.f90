!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : ENDSpecRead02 - WAIT() Statement
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March  8, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() on Pending Read Data Transfer
!*                               Operations (with/without ID= Specifier)
!*                               for a Specific Unit
!*  SECONDARY FUNCTIONS TESTED : END= Specifier (end-of-file Condition
!*                               encountered)
!*
!*  DRIVER STANZA              : xlf2003
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

PROGRAM ENDSpecRead02
    USE ISO_FORTRAN_ENV

    INTEGER, DIMENSION( 10 ) :: ioID
    INTEGER, DIMENSION( 5,10 ) :: dataValues

    CHARACTER(LEN = 256) :: iMsg


    OPEN(811, ASYNCHRONOUS='yes', ACTION='read',&
        &ACCESS='sequential', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    i = 0
    DO WHILE ((i < 10)  .AND.  (iStat <> IOSTAT_END))
        i = i + 1

        READ(811, '(5I4)', ASYNCHRONOUS='yes', ID=ioID( i ),&
            &IOSTAT=iStat, IOMSG=iMsg) (dataValues( j,i ), j = 1, 5)

        IF (iStat <> 0) THEN
            WRITE(6, *) "READ() <", iStat, "> ", iMsg

            IF (iStat <> IOSTAT_END) THEN
                CALL zzrc( (i + 10) )
            END IF
        END IF
    END DO


    WAIT(IOSTAT=iStat, UNIT=811, ID=ioID( i ), END=100, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        CALL zzrc( (i + 20) )
    END IF


    WAIT(IOSTAT=iStat, UNIT=811, END=200, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        CALL zzrc( 31 )
    END IF

    GOTO 300


100 CONTINUE
    WRITE(6, *) i, ") WAIT(", ioID( i ), ") <", iStat, "> ", iMsg
    GOTO 300

200 CONTINUE
    WRITE(6, *) "WAIT() <", iStat, "> ", iMsg


300 DO j = 1, i
        WRITE(6, '(I2,")",5I4)') j, (dataValues( k,j ), k = 1, 5)
    END DO


    CLOSE(811, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 41 )
    END IF

END PROGRAM ENDSpecRead02
