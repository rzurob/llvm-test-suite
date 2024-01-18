!*  ===================================================================
!*
!*  DATE                       : March  7, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement
!*  SECONDARY FUNCTIONS TESTED : ID= Specifier where the Value is a
!*                               scalar-int-expr that is a literal constant
!*                               (but a valid Identifier for the specified
!*                               Unit)
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

PROGRAM idSpecIDValue05
    IMPLICIT NONE

    INTEGER :: iID
    INTEGER :: iID1
    INTEGER :: iID2
    INTEGER :: iID3
    INTEGER :: iStat

    CHARACTER(LEN = 256) :: iMsg


    OPEN(66, ACTION='write', ACCESS='sequential', IOMSG=iMsg,&
        &FORM='unformatted', ASYNCHRONOUS='yes', IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    WRITE(66, ASYNCHRONOUS='yes', ID=iID, IOSTAT=iStat, IOMSG=iMsg) 66
    IF (iStat <> 0) THEN
        WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
        CALL zzrc( 2 )
    END IF

    iID1 = iID


    WRITE(66, ASYNCHRONOUS='yes', ID=iID, IOSTAT=iStat, IOMSG=iMsg) 66
    IF (iStat <> 0) THEN
        WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
        CALL zzrc( 3 )
    END IF

    iID2 = iID


    WRITE(66, ASYNCHRONOUS='yes', ID=iID, IOSTAT=iStat, IOMSG=iMsg) 66
    IF (iStat <> 0) THEN
        WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
        CALL zzrc( 4 )
    END IF

    iID3 = iID


    PRINT *, "s/ID=iID1,/ID=", iID1, ",/"

    WAIT(UNIT=66, ID=iID1, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        CALL zzrc( 5 )
    END IF


    PRINT *, "s/ID=iID2,/ID=", iID2, ",/"

    WAIT(66, ID=iID2, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        CALL zzrc( 6 )
    END IF


    PRINT *, "s/ID=iID3,/ID=", iID3, ",/"

    WAIT(ID=iID3, IOSTAT=iStat, UNIT=66, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        CALL zzrc( 7 )
    END IF


    CLOSE(UNIT=66, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 8 )
    END IF

END PROGRAM idSpecIDValue05
