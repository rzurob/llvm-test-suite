!*  ===================================================================
!*
!*  DATE                       : March  3, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement
!*  SECONDARY FUNCTIONS TESTED : UNIT= Specifier Value is a Unit that:
!*                               a)  does not exist,
!*                               b)  was not OPEN()ed for Asynchronous I/O,
!*                               c)  has no file connected to it
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), UNIT= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
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
!*  C939 (R922) A file-unit-number shall be specified; if the optional
!*              characters UNIT= are omitted, the file-unit-number shall
!*              be the first item in the wait-spec-list.
!*
!*  ...
!*
!*  Execution of a WAIT statement specifying a unit that does not exist, has
!*  no file connected to it, or was not opened for asynchronous input/output
!*  is permitted, provided that the WAIT statement has no ID= specifier; such
!*  a WAIT statement does not cause an error or end-of-file condition to occur.
!*
!*  9.4 File connection
!*
!*  A unit, specified by an io-unit, provides a means for referring to a file.
!*
!*  R901 io-unit           is  file-unit-number
!*                         ...
!*
!*  R902 file-unit-number  is  scalar-int-expr
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM unitSpecFileUnitNum03


    !
    !  UNIT= Specifier Value is a Unit that:
    !  a)  does not exist,
    !
    WAIT(UNIT=78, IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(6, *) "WAIT(Unit DOESNOT Exist) <", iStat, ">"
        CALL zzrc( 1 )
    END IF


    !
    !  UNIT= Specifier Value is a Unit that:
    !  b)  was not OPEN()ed for Asynchronous I/O,
    !
    OPEN(78, ASYNCHRONOUS='no', IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(6, *) "OPEN() <", iStat, ">"
        CALL zzrc( 2 )
    END IF

    WRITE(78, FMT='(I3)', IOSTAT=iStat) 78
    IF (iStat <> 0) THEN
        WRITE(6, *) "WRITE() <", iStat, ">"
        CALL zzrc( 3 )
    END IF

    WAIT(78, IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(6, *) "WAIT(ASYNCHRONOUS=no) <", iStat, ">"
        CALL zzrc( 4 )
    END IF


    !
    !  UNIT= Specifier Value is a Unit that:
    !  c)  has no file connected to it
    !
    CLOSE(78, IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(6, *) "CLOSE() <", iStat, ">"
        CALL zzrc( 5 )
    END IF

    WAIT(IOSTAT=iStat, UNIT=78)
    IF (iStat <> 0) THEN
        WRITE(6, *) "WAIT(ClosedUnit) <", iStat, ">"
        CALL zzrc( 6 )
    END IF

END PROGRAM unitSpecFileUnitNum03
