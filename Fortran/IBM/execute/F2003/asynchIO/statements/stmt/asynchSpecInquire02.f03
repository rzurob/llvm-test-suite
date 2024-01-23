!*  ===================================================================
!*
!*                               Specifier in I/O Statements
!*
!*  DATE                       : January 25, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in INQUIRE()
!*                               Statement on a file OPEN()ed for
!*                               Formatted Input
!*  SECONDARY FUNCTIONS TESTED : Value in scalar-default-char-variable is:
!*                               'YES', or 'NO'
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : INQUIRE(), ASYNCHRONOUS= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
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
!*
!*  9.9.1.4 ASYNCHRONOUS= specifier in the INQUIRE statement
!*
!*  The scalar-default-char-variable in the ASYNCHRONOUS= specifier is
!*  assigned the value YES if the file is connected and asynchronous
!*  input/output on the unit is allowed; it is assigned the value NO if
!*  the file is connected and asynchronous input/output on the unit is
!*  not allowed. If there is no connection, the scalar-default-char-variable
!*  is assigned the value UNDEFINED.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchSpecInquire02
    IMPLICIT NONE

    INTEGER :: i = 5
    INTEGER :: ioUnit = 8
    INTEGER :: ioStatus

    CHARACTER(len = 4) :: asynchOpen
    CHARACTER(len = 9) :: asynchState
    CHARACTER(len = 256) :: ioErrorMsg



    asynchOpen = 'YeS'
    OPEN(ioUnit, FILE='asynchSpecInquire02.dat',&
        &ACTION='read', ASYNCHRONOUS=asynchOpen,&
        &FORM='formatted', IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "OPEN(Asynchronous, read): ioStatus = (",&
                                &ioStatus, ") ", ioErrorMsg
        ERROR STOP 3
    END IF

    !
    !  2)  INQUIRE() on a file Opened for Asynchronous Input.
    !
    INQUIRE(UNIT=ioUnit, ASYNCHRONOUS=asynchState,&
                    &IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "INQUIRE(Asynchronous, read): ioStatus = (",&
                                        &ioStatus, ") ", ioErrorMsg
        ERROR STOP 4
    END IF

    IF (asynchState /= 'YES') THEN
        PRINT *, "INQUIRE(Asynchronous, read): asynchState = (",&
                                                    &asynchState, ")"
        ERROR STOP 5
    END IF

    CLOSE(UNIT=8, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "CLOSE(): ioStatus = (", ioStatus, ") ", ioErrorMsg
        ERROR STOP 6
    END IF



    asynchOpen = 'No'
    OPEN(FILE='asynchSpecInquire02.dat', ACTION='read',&
        &UNIT=ioUnit, FORM='formatted', ASYNCHRONOUS=asynchOpen,&
                                &IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "OPEN(Non-Asynchronous, Read): ioStatus = (",&
                                        &ioStatus, ") ", ioErrorMsg
        ERROR STOP 7
    END IF

    !
    !  2)  INQUIRE() on a file Opened for Non-Asynchronous Input.
    !
    INQUIRE(FILE='asynchSpecInquire02.dat', IOMSG=ioErrorMsg,&
                    &ASYNCHRONOUS=asynchState, IOSTAT=ioStatus)
    IF (ioStatus /= 0) THEN
        PRINT *, "INQUIRE(Non-Asynchronous, Read): ioStatus = (",&
                                        &ioStatus, ") ", ioErrorMsg
        ERROR STOP 8
    END IF

    IF (asynchState /= 'NO') THEN
        PRINT *, "INQUIRE(Non-Asynchronous, Read): asynchState = (",&
                                                        &asynchState, ")"
        ERROR STOP 9
    END IF

    CLOSE(UNIT=8, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "CLOSE(): ioStatus = (", ioStatus, ") ", ioErrorMsg
        ERROR STOP 10
    END IF


END PROGRAM asynchSpecInquire02
