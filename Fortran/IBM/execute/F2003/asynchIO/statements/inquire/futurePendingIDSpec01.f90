!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : futurePendingIDSpec01 - INQUIRE() Statement
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pending Data Transfers
!*  SECONDARY FUNCTIONS TESTED : INQUIRE() with both the PENDING= and ID=
!*                               Specifiers; the ID= Specifier Values are
!*                               for Pending Data Transfers that have yet
!*                               to occur
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : INQUIRE(), ID= Specifier, PENDING= Specifier
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
!*                     or  ERR = label
!*  ...
!*                     or  ID = scalar-int-expr
!*  ...
!*                     or  IOSTAT = scalar-int-variable
!*  ...
!*                     or  PENDING = scalar-default-logical-variable
!*  ...
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

PROGRAM futurePendingIDSpec01

    INTEGER, DIMENSION( 10 ) :: ioID

    LOGICAL :: know

    CHARACTER(len = 256) :: iMsg = ''


    OPEN(947, ACTION='write', FORM='unformatted',&
            ACCESS='sequential', ASYNCHRONOUS='yes')


    do i = 1, 10
        WRITE(947, ASYNCHRONOUS='yes', ID=ioID( i )) ((i * 271) + 91)
    end do


    call InquireOnIDs(947, ioID)


    do i = 10, 1, -1
        INQUIRE(947, ID=ioID( i ), PENDING=know, IOSTAT=iStat, IOMSG=iMsg)

        IF (iStat /= 0) THEN
            WRITE(0, *) "INQUIRE(", ioID( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( (30 + i) )

        ELSE IF ( know ) THEN
            WRITE(0, *) "INQUIRE(", ioID( i ), ",PENDING=", know, ")"
            CALL zzrc( (40 + i) )
        END IF
    end do


    CLOSE( 947 )

END PROGRAM futurePendingIDSpec01


subroutine InquireOnIDs(ioUnit, aIDs)

    INTEGER :: ioUnit
    INTEGER, DIMENSION( 10 ), INTENT(IN) :: aIDs

    INTEGER, DIMENSION( 10 ) :: localIDs

    LOGICAL :: guess

    CHARACTER(len = 256) :: iMsg = ''


    DO i = 1, 10
        localIDs( i ) = aIDs( i ) + 10

        WRITE(0, *) "aIDs(", i, ") = '", aIDs( i ), "', ",&
                    "localIDs(", i, ") = '", localIDs( i ), "'"


        INQUIRE(ioUnit, ID=localIDs( i ),&
                PENDING=guess, IOSTAT=iStat, IOMSG=iMsg)

        IF (iStat /= 226) THEN
            WRITE(0, *) "INQUIRE(", localIDs( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( (i + 10) )
        END IF
    END DO

end subroutine InquireOnIDs
