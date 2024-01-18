! *********************************************************************
!* ===================================================================
!*
!* DATE                         : Sep. 2010
!*
!* PRIMARY FUNCTIONS TESTED     : NEWUNIT in OPEN()
!* SECONDARY FUNTIONS TESTED    : BACKSPACE,READ,WRITE,WAIT
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Execution of a file positioning statement
!*                                performs a wait operation asynchronous
!*                                data transfer operations for NEWUNIT
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    PROGRAM FXOPEN_NEWUNIT16

    INTEGER :: IVAR,i
    INTEGER, DIMENSION( 2 ) :: VVAR

    CHARACTER(LEN = 256) :: iMsg


    OPEN(NEWUNIT=IVAR, FILE='fxopen-newunit16.dat', ASYNCHRONOUS='yes', &
         IOMSG=iMsg,ACCESS='sequential', ACTION='read', FORM='unformatted',&
         IOSTAT=iStat)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1_4 )
    END IF


    DO i = 1, 2
        READ(IVAR, ASYNCHRONOUS='yes', ID=ioID,&
              IOSTAT=iStat, IOMSG=iMsg) VVAR( i )
        IF (iStat /= 0) THEN
            WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
            CALL zzrc( 2_4 )
        END IF

        WAIT(IVAR, ID=ioID, IOSTAT=iStat, IOMSG=iMsg)

        IF (iStat /= 0) THEN
            WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
            ERROR STOP 4_4
        END IF

        BACKSPACE(IVAR, IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat /= 0) THEN
            WRITE(0, *) "BACKSPACE() <", iStat, "> ", iMsg
            CALL zzrc( 3_4 )
        END IF


    END DO

    CLOSE(IVAR, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 5_4 )
    END IF


    PRINT *, VVAR

    END PROGRAM FXOPEN_NEWUNIT16
