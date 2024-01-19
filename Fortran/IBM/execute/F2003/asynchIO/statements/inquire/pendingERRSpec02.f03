!*  ===================================================================
!*
!*  DATE                       : March 24, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : An Error in one of several Pending Data
!*                               Transfers
!*  SECONDARY FUNCTIONS TESTED : INQUIRE() with the PENDING=, and ERR=
!*                               Specifiers
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), INQUIRE(), PENDING= Specifier,
!*                               ERR= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.6.2 Wait operation
!*
!*  If an error or end-of-file condition occurs during a wait operation for
!*  a unit, the processor performs a wait operation for all pending data
!*  transfer operations for that unit.
!*
!*  9.9 File inquiry
!*
!*  R929 inquire-stmt  is  INQUIRE ( inquire-spec-list )
!*
!*  9.9.1 Inquiry specifiers
!*
!*  R930 inquire-spec  is  [ UNIT = ] file-unit-number
!*                     or  FILE = file-name-expr
!*                     or  ACCESS = scalar-default-char-variable
!*                     or  ACTION = scalar-default-char-variable
!*                     or  ASYNCHRONOUS = scalar-default-char-variable
!*  ...
!*                     or  ERR = label
!*  ...
!*                     or  PENDING = scalar-default-logical-variable
!*  ...
!*
!*  C950 (R930) If an ID= specifier appears, a PENDING= specifier shall
!*              also appear.
!*
!*  9.9.1.20 PENDING= specifier in the INQUIRE statement
!*
!*  The PENDING= specifier is used to determine whether or not previously
!*  pending asynchronous data transfers are complete. A data transfer
!*  operation is previously pending if it is pending at the beginning of
!*  execution of the INQUIRE statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM pendingERRSpec02

    INTEGER, DIMENSION( 10 ) :: dataValues
    INTEGER, DIMENSION( 10 ) :: recNum = (/ (i, i = 1, 10) /)

    LOGICAL :: prepending

    CHARACTER(LEN = 256) :: iMsg


    OPEN(211, RECL=4, ACCESS='direct', ACTION='read',&
        FORM='unformatted', FILE='pendingERRSpec02.dat',&
            ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=iMsg)
    IF (0 /= iStat) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    dataValues = 0
    DO i = 1, 10
        recNum( 5 ) = recNum( 5 ) * i

        READ(211, ASYNCHRONOUS='yes', REC=recNum( i ),&
                IOSTAT=iStat, IOMSG=iMsg) dataValues( i )

        IF (0 /= iStat) THEN
            WRITE(0, *) i, "READ() <", iStat, "> ", iMsg
            CALL zzrc( 10 + i )
        END IF
    END DO


    INQUIRE(211, ERR=100, PENDING=prepending, IOSTAT=iStat, IOMSG=iMsg)

    WRITE(0, *) "INQUIRE() <", iStat, "> ", iMsg
    CALL zzrc( 21 )


100 WRITE(6, *) "INQUIRE(ERR=100) <", iStat, "> ", iMsg
    WRITE(6, '(10I6)') (dataValues( i ), i = 1, 10)


    CLOSE(211, IOSTAT=iStat, IOMSG=iMsg)
    IF (0 /= iStat) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 31 )
    END IF

END PROGRAM pendingERRSpec02
