!*  ===================================================================
!*
!*  DATE                       : March 28, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : INQUIRE() using the FILE= Specifier for a
!*                               File that has *NOT* been OPEN()ed
!*  SECONDARY FUNCTIONS TESTED : PENDING= and ID= Specifiers are present
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : INQUIRE(), FILE= Specifier, PENDING=
!*                               Specifier, ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
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
!*                     or  ID = scalar-int-expr
!*  ...
!*                     or  PENDING = scalar-default-logical-variable
!*  ...
!*
!*  C950 (R930) If an ID= specifier appears, a PENDING= specifier shall
!*              also appear.
!*
!*  9.9.1.13 ID= specifier in the INQUIRE statement
!*
!*  The value of the expression specified in the ID= specifier shall be the
!*  identifier of a pending data transfer operation for the specified unit.
!*
!*  9.9.1.20 PENDING= specifier in the INQUIRE statement
!*
!*  The PENDING= specifier is used to determine whether or not previously
!*  pending asynchronous data transfers are complete. A data transfer
!*  operation is previously pending if it is pending at the beginning of
!*  execution of the INQUIRE statement.
!*
!*  If an ID= specifier appears and the specified data transfer operation
!*  is complete, then the variable specified in the PENDING= specifier is
!*  assigned the value false and the INQUIRE statement performs the wait
!*  operation for the specified data transfer.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program nonAsynchPendingSpec03

    INTEGER, DIMENSION( 10 ) :: ioID

    LOGICAL :: pretending

    CHARACTER(LEN = 256) :: iMsg


    OPEN(2044, FORM='unformatted',&
        FILE='nonAsynchPendingSpec03.txt',&
        ASYNCHRONOUS='yes', ACCESS='stream', ACTION='write')


    DO i = 1, 10
        WRITE(2044, ASYNCHRONOUS='yes',&
            ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg) i

        IF (iStat /= 0) THEN
            WRITE(0, *) i, "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( i + 10 )
        END IF
    END DO


    INQUIRE(FILE='nonAsynchPendingSpec03.dat',&
        PENDING=pretending, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "INQUIRE() <", iStat, "> ", iMsg
        CALL zzrc( 21 )

    ELSE IF ( pretending ) THEN
        WRITE(0, *) "INQUIRE(PENDING=", pretending, ")"
        CALL zzrc( 31 )
    END IF


    DO i = 10, 1, -1
        INQUIRE(FILE='nonAsynchPendingSpec03.dat',&
            ID=ioID( i ), PENDING=pretending, IOSTAT=iStat, IOMSG=iMsg)

        IF (iStat /= 231) THEN
            WRITE(0, *) i, "INQUIRE(ID=", ioID( i ),&
                                ") <", iStat, "> ", iMsg
            CALL zzrc( i + 40 )

        ELSE IF ( pretending ) THEN
            WRITE(0, *) i, "INQUIRE(ID=", ioID( i ),&
                            ",PENDING=", pretending, ")"
            CALL zzrc( i + 50 )
        END IF

        INQUIRE(FILE='nonAsynchPendingSpec03.txt',&
            ID=ioID( i ), PENDING=pretending, IOSTAT=iStat, IOMSG=iMsg)

        IF (iStat /= 0) THEN
            WRITE(0, *) i, "INQUIRE(ID=", ioID( i ),&
                                ") <", iStat, "> ", iMsg
            CALL zzrc( i + 60 )
        END IF
    END DO

    CLOSE( 2044 )

end program nonAsynchPendingSpec03
