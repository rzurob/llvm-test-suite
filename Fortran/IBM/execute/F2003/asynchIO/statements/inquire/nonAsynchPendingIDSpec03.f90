!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : INQUIRE() with the both the PENDING= and ID=
!*                               Specifiers
!*  SECONDARY FUNCTIONS TESTED : file-unit-number has *NOT* been OPEN()ed
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE(), INQUIRE(), PENDING= Specifier,
!*                               ID= Specifier
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
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM nonAsynchPendingIDSpec03

    INTEGER :: noIOID = 1

    LOGICAL :: isPending = .false.

    CHARACTER(LEN = 256) :: iMsg = ' '


    INQUIRE(948, PENDING=isPending, ID=noIOID, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 226) THEN
        WRITE(0, *) "INQUIRE(", noIOID, ") <", iStat, "> ", iMsg
        CALL zzrc( 11 )

    ELSE IF ( isPending ) THEN
        WRITE(0, *) "INQUIRE(", noIOID, "PENDING=", isPending, ")"
        CALL zzrc( 12 )
    END IF


    INQUIRE(948, PENDING=isPending, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "INQUIRE() <", iStat, "> ", iMsg
        CALL zzrc( 13 )

    ELSE IF ( isPending ) THEN
        WRITE(0, *) "INQUIRE(PENDING=", isPending, ")"
        CALL zzrc( 14 )
    END IF

END PROGRAM nonAsynchPendingIDSpec03
