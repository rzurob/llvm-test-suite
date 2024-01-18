!*  ===================================================================
!*
!*  DATE                       : March 17, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Perform Unformatted Asynchronous WRITE()
!*                               Operations
!*  SECONDARY FUNCTIONS TESTED : Perform an INQUIRE() on each Pending Data
!*                               Transfer
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
!*  If an ID= specifier appears and the specified data transfer operation
!*  is complete, then the variable specified in the PENDING= specifier is
!*  assigned the value false and the INQUIRE statement performs the wait
!*  operation for the specified data transfer.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM pendingIDSpec01
    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: iStat

    INTEGER, DIMENSION( 100 ) :: ioID
    INTEGER, DIMENSION( 100 ) :: dataValue

    INTEGER :: ioUnit = 3552

    LOGICAL, DIMENSION( 100 ) :: depending

    CHARACTER(LEN = 256) :: iMsg


    OPEN(ioUnit, ASYNCHRONOUS='yes', FORM='unformatted',&
            &FILE='pendingIDSpec01.dat', ACTION='write',&
            &ACCESS='direct', RECL=4, IOSTAT=iStat, IOMSG=iMsg)
    IF (0 /= iStat) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    dataValue = (/ (((i * 123) + 456), i = 1, 100) /)


    DO i = 1, 100
        WRITE(ioUnit, ASYNCHRONOUS='yes', REC=(101 - i),&
            &ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg) dataValue( i )

        IF (0 /= iStat) THEN
            WRITE(0, *) i, ") WRITE() <", iStat, "> ", iMsg
            CALL zzrc( 2 )
        END IF
    END DO


    CALL DumpData( dataValue )


    DO i = 100, 1, -1
        INQUIRE(ioUnit, PENDING=depending( i ),&
            &ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg)

        IF (0 /= iStat) THEN
            WRITE(0, *) i, ") INQUIRE() <", iStat, "> ", iMsg
            CALL zzrc( 3 )
        END IF
    END DO


    DO i = 1, 100
        iMsg = ''

        WAIT(ioUnit, ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg)

        IF ( depending( i ) ) THEN
            IF (0 /= iStat) THEN
                WRITE(0, *) i, ") WAIT() <", iStat, "> ", iMsg
                CALL zzrc( 4 )
            END IF

        ELSE
            IF (224 /= iStat) THEN
                WRITE(0, *) i, ") WAIT() <", iStat, "> ", iMsg
                CALL zzrc( 5 )
            END IF
        END IF
    END DO


    CLOSE(ioUnit, IOSTAT=iStat, IOMSG=iMsg)
    IF (0 /= iStat) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 6 )
    END IF


    CONTAINS


        SUBROUTINE DumpData( dataValue )
            IMPLICIT NONE

            INTEGER, ASYNCHRONOUS, DIMENSION( 100 ), INTENT(IN) :: dataValue

            INTEGER :: i
            INTEGER :: j


            DO i = 1, 100, 10
                WRITE(6, '(10I5)') (dataValue( j ), j = i, (i + 10))
            END DO

        END SUBROUTINE DumpData

END PROGRAM pendingIDSpec01
