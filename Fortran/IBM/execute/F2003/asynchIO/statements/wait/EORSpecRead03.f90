!*  ===================================================================
!*
!*  DATE                       : March  8, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() on Pending Non-Advancing Read Data
!*                               Transfer Operations (with/without ID=
!*                               Specifier) for a Specific Unit for a
!*                               Sequential File
!*  SECONDARY FUNCTIONS TESTED : EOR= Specifier (end-of-record Condition
!*                               is encountered)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), EOR= Specifier, ID= Specifier
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
!*  An EOR= specifier has no effect if the pending data transfer operation
!*  is not a nonadvancing read.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM EORSpecRead03
    USE ISO_FORTRAN_ENV

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j

    INTEGER, DIMENSION( 10 ) :: aID
    INTEGER, DIMENSION( 10 ) :: dataValues

    INTEGER :: iStat
    CHARACTER(LEN = 256) :: iMsg


    OPEN(2001, ASYNCHRONOUS='yes', ACCESS='sequential',&
        &ACTION='read', FORM='formatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    i = 0
    DO WHILE ((i < 10)  .AND.&
                &(iStat <> IOSTAT_END)  .AND.&
                &(iStat <> IOSTAT_EOR))
        i = i + 1

        READ(2001, '(I3)', ASYNCHRONOUS='yes', ID=aID( i ),&
            &ADVANCE='no', IOSTAT=iStat, IOMSG=iMsg) dataValues( i )

        IF (iStat <> 0) THEN
            WRITE(6, *) i, ") READ() <", iStat, "> ", iMsg

            IF (iStat <> IOSTAT_EOR) THEN
                CALL zzrc( (10 + i) )
            END IF
        END IF
    END DO


    DO j = i, 1, -1
        WAIT(2001, ID=aID( j ), EOR=100, IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat <> 0) THEN
            WRITE(0, *) j, ") WAIT(", aID( j ), ") <", iStat, "> ", iMsg
            CALL zzrc( (20 + j) )
        END IF
    END DO

    GOTO 200

100 WRITE(6, *) j, ") EOR WAIT(", aID( j ), ") <", iStat, "> ", iMsg

200 CONTINUE


    DO j = 1, i
        WRITE(6, '(I2,")",I4)') j, dataValues( j )
    END DO


    CLOSE(2001, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 31 )
    END IF

END PROGRAM EORSpecRead03
