!*  ===================================================================
!*
!*  DATE                       : March 15, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() on 1st of 3 Pending (Unformatted)
!*                               Data Transfers experiences an Error
!*                               Condition
!*  SECONDARY FUNCTIONS TESTED : WAIT() on remaining Pending Data
!*                               Transfers
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
!*  ...
!*
!*  If an error or end-of-file condition occurs during a wait operation
!*  for a unit, the processor performs a wait operation for all pending
!*  data transfer operations for that unit.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM pendingError01

    INTEGER, DIMENSION( 3 ) :: aID
    INTEGER, DIMENSION( 3 ) :: dataValues = 0

    INTEGER, DIMENSION( 3 ) :: recList = (/ 100, 1, 2 /)

    CHARACTER(LEN = 256) :: iMsg

    OPEN(6043, ACTION='read', FORM='unformatted',&
        &ASYNCHRONOUS='yes', IOSTAT=iStat, RECL=4,&
        &ACCESS='direct', FILE='pendingError01.dat', IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 3
        READ(6043, ID=aID( i ), ASYNCHRONOUS='yes',&
            &REC=recList( i ), IOSTAT=iStat, IOMSG=iMsg) dataValues( i )

        IF (iStat <> 0) THEN
            WRITE(0, *) "READ() <", iStat, "> ", iMsg
            CALL zzrc( (10 + i) )
        END IF
    END DO


    WAIT(6043, ID=aID( 1 ), ERR=100, IOSTAT=iStat, IOMSG=iMsg)

    WRITE(0, *) "1 ) WAIT(", aID( 1 ), ") <", iStat, "> ", iMsg
    CALL zzrc( 21 )


100 DO i = 2, 3
        iMsg = ''

        WAIT(6043, ID=aID( i ), IOSTAT=iStat, IOMSG=iMsg)

        IF (iStat <> 224) THEN
            WRITE(0, *) i, ") WAIT(", aID( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( (20 + i) )
        END IF
    END DO


    WRITE(6, '(3I5)') (dataValues( i ), i = 1, 3)


    CLOSE(6043, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 30 )
    END IF

END PROGRAM pendingError01
