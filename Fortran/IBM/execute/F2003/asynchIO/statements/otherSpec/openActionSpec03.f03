!*  ===================================================================
!*
!*                               Specifier in I/O Statements
!*
!*  DATE                       : February 21, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in OPEN() Statement
!*  SECONDARY FUNCTIONS TESTED : ACTION=ReadWrite Specifier in OPEN() Statement
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : OPEN(), ASYNCHRONOUS= Specifier,
!*                               ACTION= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.4.5 The OPEN statement
!*  R904 open-stmt     is  OPEN ( connect-spec-list )
!*  R905 connect-spec  is  [ UNIT = ] file-unit-number
!*                     or  ACCESS = scalar-default-char-expr
!*                     or  ACTION = scalar-default-char-expr
!*                     or  ASYNCHRONOUS = scalar-default-char-expr
!*  ...
!*
!*  9.4.5.2 ACTION= specifier in the OPEN statement
!*
!*  The scalar-default-char-expr shall evaluate to READ, WRITE, or READWRITE.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM openActionSpec03
    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j
    INTEGER :: k
    INTEGER :: l

    INTEGER, DIMENSION( 1000,1000 ) :: aID

    INTEGER :: oStat
    INTEGER, PARAMETER :: ioUnit = 256

    CHARACTER(LEN = 256) :: oMsg


    OPEN(ioUnit, ACTION='readwrite', FORM='formatted', RECL=7,&
        &ASYNCHRONOUS='yes', ACCESS='direct', IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "OPEN() <", oStat, "> ", oMsg
        CALL zzrc( 1 )
    END IF


    l = 1
    DO i = 1, 1000
        DO j = 1000, 1, -1
            WRITE(ioUnit, FMT='(I7)', ASYNCHRONOUS='yes', ERR=100,&
                &REC=l, ID=aID( i,j ), IOSTAT=oStat, IOMSG=oMsg) (i * j)

            l = l + 1
        END DO
    END DO


    DO i = 1, 1000
        DO j = 1, 1000
            WAIT(ioUnit, ERR=300, ID=aID( i,j ), IOSTAT=oStat, IOMSG=oMsg)
        END DO
    END DO


    l = 1
    DO i = 1, 1000
        DO j = 1000, 1, -1
            READ(ioUnit, FMT='(I7)', REC=l,&
                &ERR=300, IOSTAT=oStat, IOMSG=oMsg) k

            IF (k .NE. (i * j)) THEN
                WRITE(0, *) "k =", k, "(i =", i, ", j =", j, ")"
                CALL zzrc( 5 )
            END IF

            l = l + 1
        END DO
    END DO


    CLOSE(ioUnit, IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", oStat, "> ", oMsg
        CALL zzrc( 5 )
    END IF


    GOTO 400


100 WRITE(0, *) "WRITE() <", oStat, "> ", oMsg
    CALL zzrc( 2 )


200 WRITE(0, *) "WAIT() <", oStat, "> ", oMsg
    CALL zzrc( 3 )


300 WRITE(0, *) "READ() <", oStat, "> ", oMsg
    CALL zzrc( 4 )


400 CONTINUE

END PROGRAM openActionSpec03
