!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : idSpecIDValue08 - WAIT() Statement
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March 15, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement on (Formatted) I/O Units
!*  SECONDARY FUNCTIONS TESTED : WAIT() on ID= Values that are scalar-int-expr
!*                               (both Constants and simple Expressions)
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), ID= Specifier
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
!*  The value of the expression specified in the ID= specifier shall be the
!*  identifier of a pending data transfer operation for the specified unit.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM idSpecIDValue08

    INTEGER, DIMENSION( 10 ) :: ioIDs
    INTEGER, DIMENSION( 10 ) :: inData
    INTEGER, DIMENSION( 10 ) :: outData

    CHARACTER(LEN = 256) :: iMsg


    OPEN(461, FILE='idSpecIDValue08.dat',&
        &ASYNCHRONOUS='yes', ACTION='readwrite',&
        &FORM='formatted', ACCESS='sequential', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 10
        outData( i ) = (i * 123) + 6

        WRITE(461, 100, ASYNCHRONOUS='yes', ID=ioIDs( i ),&
                        &IOSTAT=iStat, IOMSG=iMsg) outData( i )

        IF (iStat <> 0) THEN
            WRITE(0, *) i, ") WRITE() <", iStat, "> ", iMsg
            CALL zzrc( (10 + i) )
        END IF

        ioIDs( i ) = (ioIDs( i ) * 6) - 7
    END DO

100 FORMAT(I5)


    DO i = 10, 1, -1
        WAIT(461, ID=((ioIDs( i ) + 7) / 6), IOSTAT=iStat, IOMSG=iMsg)

        IF (iStat <> 0) THEN
            WRITE(0, *) i, ") WAIT() <", iStat, "> ", iMsg
            CALL zzrc( (20 + i) )
        END IF
    END DO

    REWIND 461


    DO i = 1, 10
        READ(461, 100, ASYNCHRONOUS='yes', ID=ioIDs( i ),&
                        &IOSTAT=iStat, IOMSG=iMsg) inData( i )

        IF (iStat <> 0) THEN
            WRITE(0, *) i, ") READ() <", iStat, "> ", iMsg
            CALL zzrc( (30 + i) )
        END IF
    END DO


    DO i = 10, 1, -1
        WAIT(UNIT=461, ID=0, IOSTAT=iStat, IOMSG=iMsg)

        IF (iStat <> 0) THEN
            WRITE(0, *) i, ") WAIT() <", iStat, "> ", iMsg
            CALL zzrc( (40 + i) )
        END IF
    END DO


    CLOSE(461, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 50 )
    END IF


    DO i = 1, 10
        IF (outData( i ) <> inData( i )) THEN
            WRITE(0, *) " inData(", i, ") = '", inData( i ), "'"
            WRITE(0, *) "outData(", i, ") = '", outData( i ), "'"

            CALL zzrc( (60 + i) )
        END IF
    END DO

END PROGRAM idSpecIDValue08
