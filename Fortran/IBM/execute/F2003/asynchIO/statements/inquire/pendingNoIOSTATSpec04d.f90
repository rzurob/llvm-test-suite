!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Pending Data Transfers with an end-of-file
!*                               Condition
!*  SECONDARY FUNCTIONS TESTED : INQUIRE() with the PENDING= Specifier; the
!*                               ID=, ERR=, and IOSTAT= Specifiers are *NOT*
!*                               present
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : INQUIRE(), PENDING= Specifier
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
!*                     or  ERR = label
!*  ...
!*                     or  IOSTAT = scalar-int-variable
!*  ...
!*                     or  PENDING = scalar-default-logical-variable
!*  ...
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

PROGRAM pendingNoIOSTATSpec04d
    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: iStat

    LOGICAL :: neverEnding

    INTEGER, dimension( 10 ) :: dataValue

    INTEGER, dimension( 10 ) :: recNum = (/ 2, 4, 6, 1, 3, 5, 10, 7, 9, 8/)

    CHARACTER(LEN = 256) :: iMsg


    open(930, FILE='pendingNoIOSTATSpec04d.dat',&
        ASYNCHRONOUS='yes', ACCESS='direct', RECL=4,&
        ACTION='read', FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
    if (iStat <> 0) THEN
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 11 )
    end if


    do i = 1, 10
        READ(930, ASYNCHRONOUS='yes', REC=recNum( i ),&
                IOSTAT=iStat, IOMSG=iMsg) dataValue( i )

        if (iStat <> 0) THEN
            write(0, *) "OPEN() <", iStat, "> ", iMsg
            call zzrc( 20 + i )
        end if
    end do


    inquire(930, PENDING=neverEnding)

    if ( neverEnding ) THEN
        write(0, *) "INQUIRE(PENDING=", neverEnding, ")"
        call zzrc( 31 )
    end if


    close(930, IOSTAT=iStat, IOMSG=iMsg)
    if (iStat <> 0) THEN
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 41 )
    end if

END PROGRAM pendingNoIOSTATSpec04d
