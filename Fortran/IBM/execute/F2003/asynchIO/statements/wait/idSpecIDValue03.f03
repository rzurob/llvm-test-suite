!*  ===================================================================
!*
!*  DATE                       : March  6, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement
!*  SECONDARY FUNCTIONS TESTED : ID= Specifier where the Value is a
!*                               scalar-int-expr
!*
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

PROGRAM idSpecIDValue03

    CHARACTER(LEN = 256) :: iMsg

    INTEGER, DIMENSION( 10000 ) :: ids
    INTEGER, DIMENSION( 10000 ) :: dataValues = (/ (i, i = 1, 10000) /)


    OPEN(UNIT=55, ACTION='write', ACCESS='direct', RECL=5,&
        &FORM='unformatted', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 10000
        WRITE(55, ASYNCHRONOUS='yes', ID=ids( i ),&
            &REC=i, IOSTAT=iStat, IOMSG=iMsg) dataValues( i )

        IF (iStat <> 0) THEN
            WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( 2 )
        END IF

        ids( i ) = (ids( i ) + 345) * 3
    END DO


    CALL Wait4IDs(55, ids)


    CLOSE(UNIT=55, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 4 )
    END IF

END PROGRAM idSpecIDValue03


SUBROUTINE Wait4IDs(ioUnit, idList)

    INTEGER :: ioUnit
    INTEGER, DIMENSION( 10000 ) :: idList

    CHARACTER(LEN = 256) :: iMsg


    DO i = 1, 10000
        WAIT(ioUnit, ID=(idList( i ) / 3 - 345), IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat <> 0) THEN
            WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
            CALL zzrc( 3 )
        END IF
    END DO

END SUBROUTINE Wait4IDs
