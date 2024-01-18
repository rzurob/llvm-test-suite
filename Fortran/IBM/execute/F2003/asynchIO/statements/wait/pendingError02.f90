!*  ===================================================================
!*
!*  DATE                       : March 17, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() on 1st, 2nd, 3rd of 3 Pending
!*                               (Unformatted) Data Transfers
!*  SECONDARY FUNCTIONS TESTED : WAIT() on 2nd Pending Data Transfer
!*                               experiences an Error Condition
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Arrays are used to track the IOSTAT=/IOMSG= Values for each
!*  Asynchronous READ() and WAIT() Operation separately.
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

PROGRAM pendingError02

    INTERFACE
        SUBROUTINE LoadData(ioUnit, dataOut)
            INTEGER, INTENT(IN) :: ioUnit
            INTEGER, DIMENSION( 3 ), INTENT(OUT) :: dataOut
        END SUBROUTINE LoadData
    END INTERFACE

    INTEGER, DIMENSION( 3 ) :: aioID
    INTEGER, DIMENSION( 3,2 ) :: aIOStat

    INTEGER, DIMENSION( 3 ) :: dataIn
    INTEGER, DIMENSION( 3 ) :: dataOut

    INTEGER, DIMENSION( 3 ) :: recIn = (/ 1, 23, 3 /)

    CHARACTER(LEN = 256) :: iMsg
    CHARACTER(LEN = 256), DIMENSION( 3,2 ) :: aIOMsg


    ioUnit = 5574
    OPEN(ioUnit, ACTION='readwrite', ACCESS='direct', RECL=4,&
        &FORM='unformatted', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    CALL LoadData(ioUnit, dataOut)


    DO i = 1, 3
        READ(ioUnit, IOSTAT=aIOStat( i,1 ), REC=recIn( i ),&
            &ID=aioID( i ), ASYNCHRONOUS='yes', IOMSG=aIOMsg( i,1 )) dataIn( i )

        IF (aIOStat( i,1 ) <> 0) THEN
            WRITE(0, *) "READ() <", aIOStat( i,1 ), "> ", aIOMsg( i,1 )
            CALL zzrc( (20 + i) )
        END IF
    END DO


    WAIT(ioUnit, ID=aioID( 1 ), IOSTAT=aIOStat( 1,2 ), IOMSG=aIOMsg( 1,2 ))
    IF (aIOStat( 1,1 ) <> 0) THEN
        WRITE(0, *) "1a) READ(", aioID( 1 ), ") <",&
                    &aIOStat( 1,1 ), "> ", aIOMsg( 1,1 )
        CALL zzrc( 31 )
    END IF

    IF (aIOStat( 1,2 ) <> 0) THEN
        WRITE(0, *) "1b) WAIT(", aioID( 1 ), ") <",&
                    &aIOStat( 1,2 ), "> ", aIOMsg( 1,2 )
        CALL zzrc( 41 )
    END IF


    WAIT(ioUnit, ID=aioID( 2 ), IOSTAT=aIOStat( 2,2 ), IOMSG=aIOMsg( 2,2 ))
    WRITE(0, *) "2a) READ(", aioID( 2 ), ") <",&
                &aIOStat( 2,1 ), "> ", aIOMsg( 2,1 )
    IF (aIOStat( 2,2 ) <> 1) THEN
        WRITE(0, *) "2b) WAIT(", aioID( 2 ), ") <",&
                    &aIOStat( 2,2 ), "> ", aIOMsg( 2,2 )
        CALL zzrc( 42 )
    END IF


    PRINT '("dataOut = (",I5,",",I5,",",I5,")")', (dataOut( i ), i = 1, 3)


    WAIT(ioUnit, ID=aioID( 3 ), IOSTAT=aIOStat( 3,2 ), IOMSG=aIOMsg( 3,2 ))
    IF (aIOStat( 3,1 ) <> 0) THEN
        WRITE(0, *) "3a) READ(", aioID( 3 ), ") <",&
                    &aIOStat( 3,1 ), "> ", aIOMsg( 3,1 )
        CALL zzrc( 33 )
    END IF

    IF (aIOStat( 3,2 ) <> 224) THEN
        WRITE(0, *) "3b) WAIT(", aioID( 3 ), ") <",&
                    &aIOStat( 3,2 ), "> ", aIOMsg( 3,2 )
        CALL zzrc( 43 )
    END IF


    PRINT '(" dataIn = (",I5,",",I5,",",I5,")")', (dataIn( i ), i = 1, 3)

    DO i = 1, 3, 2
        IF (dataOut( i ) <> dataIn( i )) THEN
            WRITE(0, *) " dataIn(", i, ") = '", dataIn( i ), "'"
            WRITE(0, *) "dataOut(", i, ") = '", dataOut( i ), "'"

            CALL zzrc( (50 + i) )
        END IF
    END DO


    CLOSE(ioUnit, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 61 )
    END IF

END PROGRAM pendingError02


SUBROUTINE LoadData(ioUnit, dataOut)

    INTEGER, INTENT(IN) :: ioUnit
    INTEGER, DIMENSION( 3 ), INTENT(OUT) :: dataOut

    CHARACTER(LEN = 256) :: iMsg


    DO i = 1, 3
        dataOut( i ) = ((i + 1929) * 13) / 5

        WRITE(ioUnit, ASYNCHRONOUS='no', REC=i,&
            &IOSTAT=iStat, IOMSG=iMsg) dataOut( i )

        IF (iStat <> 0) THEN
            WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( (10 + i) )
        END IF
    END DO

END SUBROUTINE LoadData
