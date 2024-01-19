!*  ===================================================================
!*
!*  DATE                       : March 21, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Perform Formatted Asynchronous I/O
!*                               Operations on a Unit
!*  SECONDARY FUNCTIONS TESTED : Perform an INQUIRE() with the PENDING=
!*                               Specifier (ID= Specifier is *NOT* present)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), WRITE(), INQUIRE(), PENDING=
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
!*  If the ID= specifier is omitted and all previously pending data
!*  transfer operations for the specified unit are complete, then the
!*  variable specified in the PENDING= specifier is assigned the value
!*  false and the INQUIRE statement performs wait operations for all
!*  previously pending data transfers for the specified unit.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program pendingNoIDSpec02

    INTEGER, DIMENSION( 10 ) :: ioID
    INTEGER, DIMENSION( 10 ) :: dataValues =&
                (/ ((123 + (456 * i)), i = 1, 10) /)

    LOGICAL :: isPending
    LOGICAL :: repending

    CHARACTER(LEN = 256) :: iMsg

    OPEN(650, ASYNCHRONOUS='yes', ACCESS='direct', RECL=6,&
        &ACTION='write', FORM='formatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 10
        WRITE(650, '(I6)', ASYNCHRONOUS='yes', ID=ioID( i ),&
            &REC=(i * 31), IOSTAT=iStat, IOMSG=iMsg) dataValues( i )
        IF (iStat /= 0) THEN
            WRITE(0, *) i, "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( 10 + i )
        END IF
    END DO


    INQUIRE(650, PENDING=isPending, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "INQUIRE() <", iStat, "> ", iMsg
        CALL zzrc( 21 )
    END IF


    DO i = 1, 10
        INQUIRE(650, PENDING=repending,&
            &ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg)


        IF ( isPending ) THEN
            IF (iStat /= 0) THEN
                WRITE(0, *) i, "INQUIRE(ID=", ioID( i ),&
                                    ") <", iStat, "> ", iMsg
                CALL zzrc( 30 + i )
            END IF

        ELSE
            IF (iStat /= 0) THEN
                WRITE(0, *) i, "INQUIRE(ID=", ioID( i ),&
                                    ") <", iStat, "> ", iMsg
                CALL zzrc( 40 + i )

            ELSE IF ( repending ) THEN
                WRITE(0, *) i, "INQUIRE(ID=", ioID( i ),&
                                &",PENDING=", repending, ")"
                CALL zzrc( i + 50 )
            END IF
        END IF
    END DO


    CLOSE(650, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 61 )
    END IF

end program pendingNoIDSpec02
