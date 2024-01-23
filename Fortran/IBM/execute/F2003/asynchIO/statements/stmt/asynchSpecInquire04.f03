!*  ===================================================================
!*
!*                               Specifier in I/O Statements
!*
!*  DATE                       : January 25, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : PENDING= Specifier in INQUIRE() Statement
!*  SECONDARY FUNCTIONS TESTED : ID= Specifier is *NOT* present
!*
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
!*                     or  INQUIRE ( IOLENGTH = scalar-int-variable )&
!*                                   &output-item-list
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
!*  9.9.1.20 PENDING= specifier in the INQUIRE statement
!*
!*  If the ID= specifier is omitted and all previously pending data transfer
!*  operations for the specified unit are complete, then the variable
!*  specified in the PENDING= specifier is assigned the value false and the
!*  INQUIRE statement performs wait operations for all previously pending
!*  data transfers for the specified unit.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchSpecInquire04
    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j

    INTEGER :: realPart
    INTEGER :: imagPart

    INTEGER :: asynchID
    INTEGER :: ioStatus

    LOGICAL :: pendingState

    COMPLEX, DIMENSION( 1000,1000 ) ::  cArray

    CHARACTER(len = 256) :: ioErrorMsg


    DO i = 1, 1000
        DO j = 1, 1000
            realPart = i * 3
            imagPart = 1000 - j

            cArray( j,i ) = (realPart , imagPart)
        END DO
    END DO


    OPEN(8, FILE='asynchSpecInquire04.dat',&
         &ACTION='write', ASYNCHRONOUS='yes',&
                &IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "OPEN(): ioStatus = (", ioStatus, ") ", ioErrorMsg
        ERROR STOP 3
    END IF


    WRITE(8, 10, ASYNCHRONOUS='yes', ID=asynchID,&
                &IOSTAT=ioStatus, IOMSG=ioErrorMsg)&
                &((cArray( j,i ), j = 1, 1000), i = 1, 1000)
10  FORMAT(5(' (',F6.1,',',F6.1,')'))

    PRINT *, "0)  asynchID = (", asynchID, ")"


    !
    !  INQUIRE() with PENDING= Specifier, no ID= Specifier is present.
    !
    INQUIRE(8, PENDING=pendingState, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "INQUIRE(): ioStatus = (", ioStatus, ") ", ioErrorMsg
        ERROR STOP 4
    END IF

    PRINT *, "1)  pendingState = (", pendingState, ")"


    INQUIRE(8, PENDING=pendingState, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "INQUIRE(): ioStatus = (", ioStatus, ") ", ioErrorMsg
        ERROR STOP 5
    END IF


    CLOSE(UNIT=8, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "CLOSE(): ioStatus = (", ioStatus, ") ", ioErrorMsg
        ERROR STOP 6
    END IF


END PROGRAM asynchSpecInquire04
