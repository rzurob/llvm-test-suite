!*  ===================================================================
!*
!*  DATE                       : April 11, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Unformatted Asynchronous I/O in a Sub-Program
!*                               (another Scoping Unit)
!*  SECONDARY FUNCTIONS TESTED : Perform an INQUIRE() with both the PENDING=
!*                               and ID= Specifiers on each of the Pending
!*                               Data Transfers
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE(), INQUIRE(), PENDING= Specifier,
!*                               ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  IBM Limitation -- Wait Operations need to be performed for Pending Data
!*  Transfers prior to the termination of the Scoping Unit.  Implicit Wait
!*  Operations are performed (if necessary).  This issue is documented in
!*  Defect:  317486.
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

program pendingIDSpec06

    INTERFACE
        INTEGER FUNCTION Wait4IDs(ioUnit, n, ioIDs)
            INTEGER, INTENT(IN) :: ioUnit
            INTEGER, INTENT(IN) :: n
            INTEGER, DIMENSION( n ) :: ioIDs
        END FUNCTION Wait4IDs
    END INTERFACE

    INTEGER, PARAMETER :: m = 2000
    INTEGER, PARAMETER :: n = m * 5

    INTEGER, DIMENSION( n ) :: pdtIDs
    INTEGER, DIMENSION( n ) :: dataValues = (/ (((i * 279) + 37), i = 1, n) /)

    CHARACTER(LEN = 256) :: iMsg


    OPEN(475, ASYNCHRONOUS='yes',&
        ACCESS='sequential', ACTION='write',&
        FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
    if (iStat /= 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 1_4 )
    end if


    i = 0
    j = 0
    do while ((i < n)  .AND.  (iStat == 0))
        i = i + 1
        j = j + 1

        WRITE(475, ASYNCHRONOUS='yes', ID=pdtIDs( j ),&
                IOSTAT=iStat, IOMSG=iMsg) dataValues( i )
        if (iStat /= 0) then
            write(0, *) i, "WRITE() <", iStat, "> ", iMsg

        else if (j > m) then
            iStat = Wait4IDs(475, j, pdtIDs)
            j = 0
        end if
    end do


    if (iStat /= 0) then
        call zzrc( 2_4 )


    else
        iStat = Wait4IDs(475, j, pdtIDs)
        if (iStat /= 0) then
            call zzrc( 3_4 )
        end if
    end if


    CLOSE(475, IOSTAT=iStat, IOMSG=iMsg)
    if (iStat /= 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 4_4 )
    end if

end program pendingIDSpec06


INTEGER FUNCTION Wait4IDs(ioUnit, n, ioIDs)
    INTEGER, INTENT(IN) :: ioUnit
    INTEGER, INTENT(IN) :: n
    INTEGER, DIMENSION( n ) :: ioIDs

    LOGICAL :: stillPending

    CHARACTER(len = 256) :: iMsg


    i = 0
    iStat = 0

    do while ((i < n)  .AND.  (iStat == 0))
        i = i + 1

        INQUIRE(ioUnit, PENDING=stillPending,&
            ID=ioIDs( i ), IOSTAT=iStat, IOMSG=iMsg)
        if (iStat /= 0) then
            write(0, *) i, "INQUIRE(ID=", ioIDs( i ), ") <", iStat, "> ", iMsg
        end if
    end do


    Wait4IDs = iStat

END FUNCTION Wait4IDs
