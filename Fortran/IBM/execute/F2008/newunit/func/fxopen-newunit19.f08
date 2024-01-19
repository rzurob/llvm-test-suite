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
!* DESCRIPTION                  : Using NEWUNIT with FILE and INQUIRE(),
!*                                PENDING= Specifier, ID= Specifier
!*                                ERR= Specifier,READ and unformatted
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/10/21    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    PROGRAM FXOPEN_NEWUNIT19

    INTEGER, DIMENSION( 3 ) :: ioID
    INTEGER, DIMENSION( 3 ) :: VVAR
    INTEGER :: IVAR
    LOGICAL :: stillPending

    CHARACTER(len = 256) :: iMsg

    OPEN(NEWUNIT=IVAR, ACTION='read', ACCESS='stream',&
        &FILE='fxopen-newunit19.dat', IOMSG=iMsg,&
        &ASYNCHRONOUS='yes', FORM='unformatted', IOSTAT=iStat)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL ZZRC( 1_4 )
    END IF


    DO i = 1, 3
       READ(IVAR, ID=ioID( i ), ASYNCHRONOUS='yes',&
                &IOSTAT=iStat, IOMSG=iMsg) VVAR( i )

        IF (iStat /= 0) THEN
            WRITE(0, *) i, "READ() <", iStat, "> ", iMsg
            ERROR STOP 2_4
        END IF
    END DO

    DO i = 1, 3
    WRITE(0, *) "ioID(", i, ") = '", ioID( i ), "'"
    INQUIRE(IVAR, ID=ioID( i ), PENDING=stillPending, IOMSG=iMsg, ERR=100)

    IF (i > 2) ERROR STOP 3_4
    GOTO 200
100     WRITE(0, *) "INQUIRE(ERR=100,ID=", ioID( i ), ") ", iMsg
        IF (i < 3) ERROR STOP 5_4
200     CONTINUE
    END DO



    CLOSE(IVAR, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL ZZRC( 6_4 )
    END IF

    END PROGRAM FXOPEN_NEWUNIT19
