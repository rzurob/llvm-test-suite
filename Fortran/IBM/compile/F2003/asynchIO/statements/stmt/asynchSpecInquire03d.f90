!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : asynchSpecInquire03d - ASYNCHRONOUS=
!*                               Specifier in I/O Statements
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January 25, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ID= Specifier in INQUIRE() Statement
!*  SECONDARY FUNCTIONS TESTED : PENDING= Specifier is *NOT* present
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
!*  C950 (R930) If an ID= specifier appears, a PENDING= specifier shall
!*              also appear.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchSpecInquire03d
    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j

    INTEGER :: realPart
    INTEGER :: imagPart

    INTEGER :: asynchID
    INTEGER :: ioStatus

    COMPLEX, DIMENSION( 1000,1000 ) ::  cArray

    CHARACTER(len = 256) :: ioErrorMsg


    DO i = 1, 1000
        DO j = 1, 1000
            realPart = i * 3
            imagPart = 1000 - j

            cArray( j,i ) = (realPart , imagPart)
        END DO
    END DO


    OPEN(8, FILE='asynchSpecInquire03d.dat',&
         &ACTION='write', ASYNCHRONOUS='yes',&
                &IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "OPEN(): ioStatus = (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 3 )
    END IF


    WRITE(8, 10, ASYNCHRONOUS='yes', ID=asynchID,&
                &IOSTAT=ioStatus, IOMSG=ioErrorMsg)&
                &((cArray( j,i ), j = 1, 1000), i = 1, 1000)
10  FORMAT(5(' (',F6.1,',',F6.1,')'))

    PRINT *, "0)  asynchID = (", asynchID, ")"


    !
    !  INQUIRE() with ID= Specifier, no PENDING= Specifier is present.
    !
    INQUIRE(8, ID=asynchID, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "INQUIRE(): ioStatus = (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 4 )
    END IF


    CLOSE(UNIT=8, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "CLOSE(): ioStatus = (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 7 )
    END IF


END PROGRAM asynchSpecInquire03d
