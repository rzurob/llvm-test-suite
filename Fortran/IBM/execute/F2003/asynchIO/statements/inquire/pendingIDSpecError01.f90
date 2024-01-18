!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : pendingIDSpecError01 - INQUIRE() Statement
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March 22, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : INQUIRE() with both the PENDING= and ID=
!*                               Specifiers
!*  SECONDARY FUNCTIONS TESTED : An Error Occurs on the first Pending Data
!*                               Transfer
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), INQUIRE(), PENDING= Specifier,
!*                               ID= Specifier
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

PROGRAM pendingIDSpecError01

    INTEGER, DIMENSION( 3 ) :: ioID
    INTEGER, DIMENSION( 3 ) :: dataValues = 0
    INTEGER, DIMENSION( 3 ) :: recNum = (/ 12, 2, 3 /)

    CHARACTER(LEN = 256) :: iMsg


    OPEN(2142, ASYNCHRONOUS='yes', ACCESS='direct', RECL=4,&
        ACTION='read', FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (0 /= iStat) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 3
        READ(2142, ASYNCHRONOUS='yes', ID=ioID( i ),&
            &REC=recNum( i ), IOSTAT=iStat, IOMSG=iMsg) dataValues( i )

        IF (0 /= iStat) THEN
            WRITE(0, *) "READ(REC=", recNum( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( 10 + i )
        END IF
    END DO


    CALL InquireOnID(2142, ioID)


    PRINT '(3I5)', (dataValues( i ), i = 1, 3)


    CLOSE(2142, IOSTAT=iStat, IOMSG=iMsg)
    IF (0 /= iStat) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 50 )
    END IF

END PROGRAM pendingIDSpecError01


SUBROUTINE InquireOnID(ioUnit, ids)

    INTEGER, INTENT( IN ) :: ioUnit
    INTEGER, DIMENSION( 3 ), INTENT( IN ) :: ids

    LOGICAL :: quPending

    CHARACTER(LEN = 256) :: iMsg


    DO i = 1, 3
        INQUIRE(2142, ID=ids( i ),&
            PENDING=quPending, IOSTAT=iStat, IOMSG=iMsg)

        IF ((i == 1)  .AND.  (1 /= iStat)) THEN
            WRITE(0, *) i, ")  INQUIRE(ID=", ids( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( 20 + i )

        ELSE IF ((i /= 1)  .AND.  (226 /= iStat)) THEN
            WRITE(0, *) i, ")  INQUIRE(ID=", ids( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( 30 + i )
        END IF

        IF ( quPending ) THEN
            WRITE(0, *) i, ")  INQUIRE(ID=", ids( i ),&
								",PENDING=", quPending, ")"
            CALL zzrc( 40 + i )
        END IF
    END DO

END SUBROUTINE InquireOnID
