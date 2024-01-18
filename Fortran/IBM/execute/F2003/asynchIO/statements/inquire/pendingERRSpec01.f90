!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : pendingERRSpec01 - INQUIRE() Statement
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March 24, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pending Data Transfers (no Errors)
!*  SECONDARY FUNCTIONS TESTED : INQUIRE() with the PENDING=, ID=, and
!*                               ERR= Specifiers
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), INQUIRE(), PENDING= Specifier,
!*                               ID= Specifier, ERR= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.6.2 Wait operation
!*
!*  If an error or end-of-file condition occurs during a wait operation for
!*  a unit, the processor performs a wait operation for all pending data
!*  transfer operations for that unit.
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
!*                     or  PENDING = scalar-default-logical-variable
!*  ...
!*
!*  C950 (R930) If an ID= specifier appears, a PENDING= specifier shall
!*              also appear.
!*
!*
!*  9.9.1.13 ID= specifier in the INQUIRE statement
!*
!*  The value of the expression specified in the ID= specifier shall be the
!*  identifier of a pending data transfer operation for the specified unit.
!*
!*
!*  9.9.1.20 PENDING= specifier in the INQUIRE statement
!*
!*  The PENDING= specifier is used to determine whether or not previously
!*  pending asynchronous data transfers are complete. A data transfer
!*  operation is previously pending if it is pending at the beginning of
!*  execution of the INQUIRE statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM pendingERRSpec01

    INTEGER, DIMENSION( 1000 ) :: ioID

    LOGICAL :: penn

    CHARACTER(LEN = 256) :: iMsg = ''


    OPEN(210, FORM='unformatted', ACTION='write', IOMSG=iMsg,&
        ACCESS='sequential', ASYNCHRONOUS='yes', IOSTAT=iStat)
    IF (0 /= iStat) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 1000
        WRITE(210, ASYNCHRONOUS='yes', ID=ioID( i ),&
            IOMSG=iMsg, IOSTAT=iStat) ((i * 7) + 1989)

        IF (0 /= iStat) THEN
            WRITE(0, *) i, "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( 2 )
        END IF
    END DO


    CALL Inquire4ID(210, ioID)


    INQUIRE(210, PENDING=penn, ERR=100, IOMSG=iMsg, IOSTAT=iStat)

    GOTO 200

100 WRITE(0, *) "INQUIRE(ERR=100) <", iStat, "> ", iMsg
    CALL zzrc( 4 )


200 CLOSE(210, IOMSG=iMsg, IOSTAT=iStat)
    IF (0 /= iStat) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 5 )
    END IF

END PROGRAM pendingERRSpec01


SUBROUTINE Inquire4ID(ioUnit, aID)

    INTEGER, INTENT(IN) :: ioUnit
    INTEGER, DIMENSION( 1000 ), INTENT(IN) :: aID

    LOGICAL :: penn

    CHARACTER(LEN = 256) :: iMsg = ''


    DO i = 1, 1000
        INQUIRE(210, ID=aID( i ), PENDING=penn,&
                ERR=100, IOMSG=iMsg, IOSTAT=iStat)
    END DO

    RETURN


100 WRITE(0, *) i, "INQUIRE(ERR=100,ID=", aID( i ), ") <", iStat, "> ", iMsg
    CALL zzrc( 3 )

END SUBROUTINE Inquire4ID
