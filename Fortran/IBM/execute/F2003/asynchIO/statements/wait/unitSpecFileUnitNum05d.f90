!*  ===================================================================
!*
!*  DATE                       : March  3, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement
!*  SECONDARY FUNCTIONS TESTED : ID= Specifier where the UNIT= Specifier
!*                               Value is:
!*                               a) a unit that does not exist,
!*                               b) was not opened for asynchronous,
!*                               c) has no file connected to it
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), UNIT= Specifier, ID= Specifier
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
!*  Execution of a WAIT statement specifying a unit that does not exist,
!*  has no file connected to it, or was not opened for asynchronous
!*  input/output is permitted, provided that the WAIT statement has no
!*  ID= specifier;
!*
!*  9.4 File connection
!*
!*  A unit, specified by an io-unit, provides a means for referring to a
!*  file.
!*
!*  R901 io-unit           is  file-unit-number
!*                         ...
!*
!*  R902 file-unit-number  is  scalar-int-expr
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM unitSpecFileUnitNum05d

    !
    ! Since our implementation recognizes ID=0 as valid (for Formatted
    ! Asynchronous I/O), initialize the ID= Value to an Invalid ID.
    !
    iID = -2


    !
    !  ID= Specifier where the UNIT= Specifier Value is:
    !  a) a unit that does not exist,
    !
    WAIT(27, ID=iID, IOSTAT=iStat)
    IF (iStat == 0) THEN
        CALL zzrc( 1 )
    END IF

    WRITE(6, *) "WAIT(Unit DOESNOT Exist) <", iStat, ">"


    !
    !  ID= Specifier where the UNIT= Specifier Value is:
    !  b) was not opened for asynchronous,
    !
    OPEN(IOSTAT=iStat, UNIT=27)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, ">"
        CALL zzrc( 2 )
    END IF

    WRITE(FMT='(I3)', IOSTAT=iStat, UNIT=27) 27
    IF (iStat <> 0) THEN
        WRITE(0, *) "WRITE() <", iStat, ">"
        CALL zzrc( 3 )
    END IF

    WAIT(UNIT=27, ID=iID, IOSTAT=iStat)
    IF (iStat == 0) THEN
        CALL zzrc( 4 )
    END IF

    WRITE(6, *) "WAIT(ASYNCHRONOUS=no) <", iStat, ">"


    !
    !  ID= Specifier where the UNIT= Specifier Value is:
    !  c) has no file connected to it
    !
    CLOSE(27, IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, ">"
        CALL zzrc( 5 )
    END IF

    WAIT(ID=iID, UNIT=27, IOSTAT=iStat)
    IF (iStat == 0) THEN
        CALL zzrc( 6 )
    END IF

    WRITE(6, *) "WAIT(Unit Not Connected) <", iStat, ">"


END PROGRAM unitSpecFileUnitNum05d
