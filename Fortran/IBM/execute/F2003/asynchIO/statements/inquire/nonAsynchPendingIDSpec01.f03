!*  ===================================================================
!*
!*  DATE                       : March 22, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : INQUIRE() with both the PENDING= Specifier
!*                               and ID= Specifiers
!*  SECONDARY FUNCTIONS TESTED : file-unit-number has *NOT* been OPEN()ed
!*                               for Asychronous I/O
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
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM nonAsynchPendingIDSpec01

    LOGICAL :: sPending

    CHARACTER(LEN = 512) :: iMsg


    OPEN(2048, ASYNCHRONOUS='no', FORM='unformatted',&
        &ACTION='write', ACCESS='sequential', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN(ASYNCHRONOUS=no) <", iStat, "> ", iMsg
        ERROR STOP 1
    END IF


    OPEN(2052, FORM='unformatted', FILE='nonAsynchPendingIDSpec01.dat',&
            &ACTION='read', ACCESS='sequential', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        ERROR STOP 2
    END IF


    ioID =  6543
    WRITE(2048, ASYNCHRONOUS='no', IOSTAT=iStat, IOMSG=iMsg) ioID
    IF (iStat /= 0) THEN
        WRITE(0, *) "WRITE(ASYNCHRONOUS=no) <", iStat, "> ", iMsg
        ERROR STOP 3
    END IF


    INQUIRE(2048, PENDING=sPending, ID=ioID, IOSTAT=iStat, IOMSG=iMsg)
    WRITE(6, *) "INQUIRE(ASYNCHRONOUS=no,ID=", ioID, ") <", iStat, "> ", iMsg

    IF (iStat /= 226) THEN
        ERROR STOP 4

    ELSE IF ( sPending ) THEN
        WRITE(6, *) "INQUIRE(ASYNCHRONOUS=no,ID=",&
                        ioID, ",PENDING=", sPending, ")"
        ERROR STOP 5
    END IF


    READ(2052, ASYNCHRONOUS='no', IOSTAT=iStat, IOMSG=iMsg) inData
    IF (iStat /= 0) THEN
        WRITE(0, *) "READ() <", iStat, "> ", iMsg
        ERROR STOP 6
    END IF


    INQUIRE(2052, PENDING=sPending, ID=ioID, IOSTAT=iStat, IOMSG=iMsg)
    WRITE(6, *) "INQUIRE(ID=", ioID, ") <", iStat, "> ", iMsg

    IF (iStat /= 226) THEN
        ERROR STOP 7

    ELSE IF ( sPending ) THEN
        WRITE(6, *) "INQUIRE(ID=", ioID, ",PENDING=", sPending, ")"
        ERROR STOP 8
    END IF


    CLOSE(2048, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE(ASYNCHRONOUS=no) <", iStat, "> ", iMsg
        ERROR STOP 9
    END IF


    CLOSE(2052, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        ERROR STOP 10
    END IF


    IF (ioID /= inData) THEN
        WRITE(0, *) "  ioID = '", ioID, "'"
        WRITE(0, *) "inData = '", inData, "'"

        ERROR STOP 11
    END IF

END PROGRAM nonAsynchPendingIDSpec01
