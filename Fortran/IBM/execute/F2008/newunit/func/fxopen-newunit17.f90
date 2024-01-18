! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxopen-newunit17.f
!*
!* PROGRAMMER                   : Sarah Kouchaki-Ramezan
!* DATE                         : Oct. 2010
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: NEWUNIT= specifier, Feature#:377344
!* SECONDARY FUNTIONS TESTED    : READ,WRITE,REWIND
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Execution of a file positioning statement
!*                                for NEWUNIT
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/10/21    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    PROGRAM FXOPEN_NEWUNIT17

    INTEGER, DIMENSION( 16 ) :: aID
    INTEGER, DIMENSION( 10 ) :: VVAR = (/ (i, i = 100, 118, 2) /)
    INTEGER :: IVAR,i

    CHARACTER(LEN = 256) :: iMsg


    OPEN(NEWUNIT=IVAR, ASYNCHRONOUS='yes', ACCESS='sequential',&
            ACTION='write', FILE='fxopen-newunit17.dat',&
            FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1_4 )
    END IF


    DO i = 1, 10
        WRITE(IVAR, ASYNCHRONOUS='no',&
            IOSTAT=iStat, IOMSG=iMsg) VVAR( i )

        IF (iStat <> 0) THEN
            WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( 2_4 )
        END IF
    END DO


    REWIND(IVAR, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "REWIND() <", iStat, "> ", iMsg
        CALL zzrc( 3_4 )
    END IF


    DO i = 1, 5
        WRITE(IVAR, ASYNCHRONOUS='yes', ID=aID( i ),&
            IOSTAT=iStat, IOMSG=iMsg) VVAR( i )

        IF (iStat <> 0) THEN
            WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( 4_4 )
        END IF
    END DO


    ENDFILE(IVAR, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "ENDFILE() <", iStat, "> ", iMsg
        CALL zzrc( 5_4 )
    END IF


    CLOSE(IVAR, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 6_4 )
    END IF

    END PROGRAM FXOPEN_NEWUNIT17
