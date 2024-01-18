!*  ===================================================================
!*
!*  DATE                       : March 20, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : INQUIRE() Statements with the ASYNCHRONOUS=,
!*                               ID=, and PENDING= Specifiers
!*  SECONDARY FUNCTIONS TESTED : Multiple instances of each Specifier
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : INQUIRE(), ASYNCHRONOUS= Specifier, ID=
!*                               Specifier, PENDING= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
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
!*  C947 (R930) No specifier shall appear more than once in a given
!*              inquire-spec-list.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM C947AsynchSpec01d

    LOGICAL :: depending
    LOGICAL :: expending

    CHARACTER(LEN = 9) :: asynchType

    CHARACTER(LEN = 256) :: iMsg


    OPEN(2468, ASYNCHRONOUS='yes',&
        &FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    WRITE(2468, ASYNCHRONOUS='yes', ID=ioID, IOSTAT=iStat, IOMSG=iMsg) 2468
    IF (iStat /= 0) THEN
        WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
        CALL zzrc( 2 )
    END IF


    !
    !  Multiple Instances of the ASYNCHRONOUS= Specifier
    !
    INQUIRE(ASYNCHRONOUS=asynchType, UNIT=2468, PENDING=depending,&
        &ID=ioID, ASYNCHRONOUS=asynchType, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "INQUIRE() <", iStat, "> ", iMsg
        CALL zzrc( 3 )
    END IF


    !
    !  Multiple Instances of the ID= Specifier
    !
    INQUIRE(ASYNCHRONOUS=asynchType, ID=ioID, UNIT=2468,&
        &IOSTAT=iStat, PENDING=depending, ID=ioID, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "INQUIRE() <", iStat, "> ", iMsg
        CALL zzrc( 4 )
    END IF


    !
    !  Multiple Instances of the PENDING= Specifier
    !
    INQUIRE(IOSTAT=iStat, IOMSG=iMsg, PENDING=depending, UNIT=2468,&
        &ID=ioID, ASYNCHRONOUS=asynchType, PENDING=expending)
    IF (iStat /= 0) THEN
        WRITE(0, *) "INQUIRE() <", iStat, "> ", iMsg
        CALL zzrc( 5 )
    END IF


    CLOSE(2468, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 6 )
    END IF

END PROGRAM C947AsynchSpec01d
