!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Unformatted Pending Data Transfers with
!*                               an Error Condition
!*  SECONDARY FUNCTIONS TESTED : WAIT() with ERR= Specifier (both with
!*                               and without the ID= Specifier); IOSTAT=
!*                               Specifier is *NOT* present
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), ERR= Specifier, ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                :
!*
!*  9.6.1 WAIT statement
!*
!*  A WAIT statement performs a wait operation for specified pending
!*  asynchronous data transfer operations.
!*
!*  R921 wait-stmt  is  WAIT (wait-spec-list)
!*  R922 wait-spec  is  [ UNIT = ] file-unit-number
!*                  or  END = label
!*                  or  EOR = label
!*                  or  ERR = label
!*                  or  ID = scalar-int-expr
!*                  or  IOMSG = iomsg-variable
!*                  or  IOSTAT = scalar-int-variable
!*
!*  ...
!*
!*  If any error, end-of-file, or end-of-record conditions occur, the
!*  applicable actions specified by the IOSTAT=, IOMSG=, ERR=, END=,
!*  and EOR= specifiers of the statement that performs the wait oper-
!*  ation are taken.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM ERRSpecRead02

    INTEGER, DIMENSION( 5 ) :: ioID
    INTEGER, DIMENSION( 10 ) :: dataValue


    OPEN(922, ACCESS='DIRECT', ACTION='READ',&
        RECL=4, FORM='UNFORMATTED', ASYNCHRONOUS='YES')


    dataValue = -999

    DO i = 1, 5
        READ(922, REC=i, ASYNCHRONOUS='YES', ID=ioID( i )) dataValue( i )
    END DO


    WAIT(922, ERR=100)

    WRITE(0, *) "WAIT() Failed to use ERR=100"
    ERROR STOP 11



100 DO i = 1, 5
        READ(922, REC=i, ASYNCHRONOUS='YES', ID=ioID( i )) dataValue( i + 5 )
    END DO


    DO i = 5, 1, -1
        WAIT(922, ID=ioID( i ), ERR=200)

        WRITE(0, *) "WAIT(ID=", ioID( i ), ") Failed to use ERR=200"
        CALL zzrc( 20 + i )

200     CONTINUE
    END DO

    WRITE(6, '(10I5)') (dataValue( i ), i = 1, 10)


    CLOSE( 922 )

END PROGRAM ERRSpecRead02
