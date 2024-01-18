!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : asynchIOqintsize01 - Miscellaneous Tests
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : April 12, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Asynchronous I/O
!*  SECONDARY FUNCTIONS TESTED : Default/-qintsize=[2|4|8]
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : asynchIOqintsize01:   None (Default)
!*                               asynchIOqintsize01a:  -qintsize=2
!*                               asynchIOqintsize01b:  -qintsize=4
!*                               asynchIOqintsize01c:  -qintsize=8
!*
!*  KEYWORD(S)                 : OPEN(), INQUIRE(), WAIT(), ID= Specifier,
!*                               ASYNCHRONOUS= Specifier, PENDING= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  To test Asynchronous I/O using the different variations of Default
!*  Integer Sizes.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchIOqintsize01

    INTERFACE
        INTEGER FUNCTION Wait4IDs(ioUnit, n, m, aIDs)
            INTEGER, INTENT(in) :: ioUnit
            INTEGER, INTENT(in) :: n
            INTEGER, INTENT(in) :: m
            INTEGER, DIMENSION( 10000 ), INTENT(in) :: aIDs
        END FUNCTION Wait4IDs

        INTEGER FUNCTION InquireOnIDs(ioUnit, n, m, aIDs)
            INTEGER, INTENT(in) :: ioUnit
            INTEGER, INTENT(in) :: n
            INTEGER, INTENT(in) :: m
            INTEGER, DIMENSION( 10000 ), INTENT(in) :: aIDs
        END FUNCTION InquireOnIDs
    END INTERFACE


    INTEGER(KIND = 4) :: j

    INTEGER, DIMENSION( 10000 ) :: ioID

    CHARACTER(LEN = 256) :: iMsg


    OPEN(204, ACTION='write', ACCESS='direct',&
        FORM='unformatted', ASYNCHRONOUS='yes',&
                RECL=4, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1_4 )
    END IF


    j = 0
    DO i = 1, 10000
        j = j + 1

        WRITE(204, ASYNCHRONOUS='yes', REC=i, ID=ioID( i ),&
                    IOSTAT=iStat, IOMSG=iMsg) ((j * 51_4) + 49_4)

        IF (iStat /= 0) THEN
            WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( 2_4 )
        END IF
    END DO


    iStat = Wait4IDs(204, 1, 5000, ioID)
    IF (iStat /= 0) THEN
        CALL zzrc( 3_4 )
    END IF


    iStat = InquireOnIDs(204, 5001, 10000, ioID)
    IF (iStat /= 0) THEN
        CALL zzrc( 4_4 )
    END IF


    CLOSE(204, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 5_4 )
    END IF

END PROGRAM asynchIOqintsize01


INTEGER FUNCTION Wait4IDs(ioUnit, n, m, aIDs)
    INTEGER, INTENT(in) :: ioUnit
    INTEGER, INTENT(in) :: n
    INTEGER, INTENT(in) :: m
    INTEGER, DIMENSION( 10000 ), INTENT(in) :: aIDs

    CHARACTER(LEN = 256) :: iMsg


    i = m
    iStat = 0

    DO WHILE ((i < n)  .AND.  (iStat == 0))
        WAIT(ioUnit, ID=aIDs( i ), IOSTAT=iStat, IOMSG=iMsg)

        IF (iStat /= 0) THEN
            WRITE(0, *) i, ")  WAIT(ID=", aIDs( i ), ") <", iStat, "> ", iMsg
        END IF

        i = i + 1
    END DO


    Wait4IDs = iStat

END FUNCTION Wait4IDs


INTEGER FUNCTION InquireOnIDs(ioUnit, n, m, aIDs)
    INTEGER, INTENT(in) :: ioUnit
    INTEGER, INTENT(in) :: n
    INTEGER, INTENT(in) :: m
    INTEGER, DIMENSION( 10000 ), INTENT(in) :: aIDs

    LOGICAL :: isPending

    CHARACTER(LEN = 256) :: iMsg


    i = m
    iStat = 0

    DO WHILE ((i < n)  .AND.  (iStat == 0))
        INQUIRE(ioUnit, PENDING=isPending,&
                ID=aIDs( i ), IOSTAT=iStat, IOMSG=iMsg)

        IF (iStat /= 0) THEN
            WRITE(0, *) i, ")  INQUIRE(ID=",&
                            aIDs( i ), ") <", iStat, "> ", iMsg

        ELSE IF ( isPending ) THEN
            iStat = 1

            WRITE(0, *) i, ")  INQUIRE(ID=",&
                            aIDs( i ), ",PENDING=", isPending, ")"
        END IF

        i = i + 1
    END DO


    InquireOnIDs = iStat

END FUNCTION InquireOnIDs
