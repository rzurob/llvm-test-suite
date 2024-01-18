! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : NEWUNIT with REWIND,READ,WRITE
!* TEST CASE TITLE              : - NEWUNIT comes with FILE
!*
!* PROGRAMMER                   : Sarah Kouchaki-Ramezan
!* DATE                         : Sep. 2010
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : NEWUNIT in OPEN()
!* SECONDARY FUNTIONS TESTED    : BACKSPACE,READ,WRITE,WAIT
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Execution of a FILE positioning statement
!*                                perFORMs a wait operation ASYNCHRONOUS
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

    PROGRAM fxopen_newunit18

    INTEGER, DIMENSION( 3 ) :: VVAR
    INTEGER, DIMENSION( 3 ) :: ioID
    INTEGER :: IVAR

    CHARACTER(len = 256) :: iMsg

    OPEN(NEWUNIT=IVAR, ACTION='readwrite', ACCESS='stream',&
        &FILE='fxopen-newunit18.dat', IOMSG=iMsg,&
        &ASYNCHRONOUS='yes', FORM='unformatted', IOSTAT=iStat)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL ZZRC( 1_4 )
    END IF

      IF ( IVAR >= -2 ) THEN
      ERROR STOP 1_4
      ENDIF


    DO i = 1, 3
       WRITE(IVAR, ID=ioID( i ), ASYNCHRONOUS='yes',&
                &IOSTAT=iStat, IOMSG=iMsg) VVAR( i )

        IF (iStat /= 0) THEN
            WRITE(0, *) i, "READ() <", iStat, "> ", iMsg
            ERROR STOP 2_4
        END IF
    END DO

    REWIND(IVAR)
    BACKSPACE(IVAR)

    DO i = 1, 3
        READ(IVAR, ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=iMsg,  ID=ioID( i )) VVAR( i )
        IF (iStat /= 0) then
            WRITE(0, *) i, "READ() <", iStat, "> ", iMsg
            CALL ZZRC( 5_4 )
        END IF
    END DO

    CLOSE(IVAR, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) then
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL ZZRC( 6_4 )
    END IF


    END PROGRAM fxopen_newunit18
