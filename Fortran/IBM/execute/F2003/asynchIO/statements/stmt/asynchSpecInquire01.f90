!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : asynchSpecInquire01 - ASYNCHRONOUS=
!*                               Specifier in I/O Statements
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January 25, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in INQUIRE()
!*                               Statement on a file OPEN()ed for
!*                               Formatted Output
!*  SECONDARY FUNCTIONS TESTED : Value in scalar-default-char-variable is:
!*                               'YES', 'NO', or 'UNDEFINED'
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : INQUIRE(), ASYNCHRONOUS= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
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

PROGRAM asynchSpecInquire01
    IMPLICIT NONE

    INTEGER :: i = 5
    INTEGER :: ioStatus

    CHARACTER(len = 9) :: asynchState
    CHARACTER(len = 256) :: ioErrorMsg


    !
    !  1)  INQUIRE() on a file with No Connnection.
    !
    INQUIRE(UNIT=8, ASYNCHRONOUS=asynchState,&
            &IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "INQUIRE(No Connection): ioStatus = (",&
                                &ioStatus, ") ", ioErrorMsg
        CALL zzrc( 1 )
    END IF

    IF (asynchState /= 'UNDEFINED') THEN
        PRINT *, "INQUIRE(No Connection): asynchState = (", asynchState, ")"
        CALL zzrc( 2 )
    END IF



    OPEN(8, ACTION='write', ASYNCHRONOUS='yEs  ',&
        &FORM='formatted', IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "OPEN(Asynchronous, Write): ioStatus = (",&
                                &ioStatus, ") ", ioErrorMsg
        CALL zzrc( 3 )
    END IF

    !
    !  2)  INQUIRE() on a file Opened for Asynchronous Output.
    !
    INQUIRE(UNIT=8, ASYNCHRONOUS=asynchState,&
            &IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "INQUIRE(Asynchronous, Write): ioStatus = (",&
                                        &ioStatus, ") ", ioErrorMsg
        CALL zzrc( 4 )
    END IF

    IF (asynchState /= 'YES') THEN
        PRINT *, "INQUIRE(Asynchronous, Write): asynchState = (",&
                                                    &asynchState, ")"
        CALL zzrc( 5 )
    END IF

    CLOSE(8, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "CLOSE(): ioStatus = (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 6 )
    END IF



    OPEN(8, ACTION='write', ASYNCHRONOUS='nO',&
        &FORM='formatted', IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "OPEN(Non-Asynchronous, Write): ioStatus = (",&
                                        &ioStatus, ") ", ioErrorMsg
        CALL zzrc( 7 )
    END IF

    !
    !  3)  INQUIRE() on a file Opened for Non-Asynchronous Output.
    !
    INQUIRE(UNIT=8, ASYNCHRONOUS=asynchState,&
            &IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "INQUIRE(Non-Asynchronous, Write): ioStatus = (",&
                                        &ioStatus, ") ", ioErrorMsg
        CALL zzrc( 8 )
    END IF

    IF (asynchState /= 'NO') THEN
        PRINT *, "INQUIRE(Non-Asynchronous, Write): asynchState = (",&
                                                        &asynchState, ")"
        CALL zzrc( 9 )
    END IF

    CLOSE(8, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "CLOSE(): ioStatus = (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 10 )
    END IF


END PROGRAM asynchSpecInquire01
