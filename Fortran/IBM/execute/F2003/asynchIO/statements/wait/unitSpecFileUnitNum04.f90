!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : unitSpecFileUnitNum04 - WAIT() Statement
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March  3, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement
!*  SECONDARY FUNCTIONS TESTED : UNIT= Specifier Value is:
!*                               a)  INPUT_UNIT (Unit=5),
!*                               b)  OUTPUT_UNIT (Unit=6),
!*                               c)  ERROR_UNIT (Unit=0)
!*
!*  DRIVER STANZA              : xlf2003
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
!*  Execution of a WAIT statement specifying a unit that ... or was not
!*  opened for asynchronous input/output is permitted, provided that the
!*  WAIT statement has no ID= specifier; ...
!*
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
!*  ...
!*
!*  A unit is either an external unit or an internal unit. An external unit
!*  is used to refer to an external file and is specified by an asterisk or
!*  a file-unit-number whose value is nonnegative or equal to one of the
!*  named constants INPUT_UNIT, OUTPUT_UNIT, or ERROR_UNIT of the
!*  ISO_FORTRAN_ENV module (13.8.2).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM unitSpecFileUnitNum04
    USE ISO_FORTRAN_ENV 


    !
    !  UNIT= Specifier Value is:
    !  a)  INPUT_UNIT (Unit=5),
    !
    READ(UNIT=5, FMT=100, IOSTAT=iStat) i
    IF (iStat <> 0) THEN
        WRITE(0, *) "READ(5) <", iStat, ">"
        CALL zzrc( 1 )
    END IF

    WAIT(INPUT_UNIT, IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT(INPUT_UNIT) <", iStat, ">"
        CALL zzrc( 2 )
    END IF


    READ(IOSTAT=iStat, FMT=100, UNIT=INPUT_UNIT) j
    IF (iStat <> 0) THEN
        WRITE(0, *) "READ(INPUT_UNIT) <", iStat, ">"
        CALL zzrc( 3 )
    END IF

    WAIT(IOSTAT=iStat, UNIT=5)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT(5) <", iStat, ">"
        CALL zzrc( 4 )
    END IF


    IF (i <> j) THEN
        WRITE(0, *) "(i != j)  i = '", i, "', j = '", j, "'"
        CALL zzrc( 5 )

    ELSE IF (i <> 5) THEN
        WRITE(0, *) "(i != 5)  i = '", i, "'"
        CALL zzrc( 6 )
    END IF


    !
    !  UNIT= Specifier Value is:
    !  b)  OUTPUT_UNIT (Unit=6),
    !
    WRITE(6, IOSTAT=iStat, FMT=100) i
    IF (iStat <> 0) THEN
        WRITE(0, *) "WRITE(6) <", iStat, ">"
        CALL zzrc( 7 )
    END IF

    WAIT(UNIT=OUTPUT_UNIT, IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT(OUTPUT_UNIT) <", iStat, ">"
        CALL zzrc( 8 )
    END IF


    WRITE(IOSTAT=iStat, FMT=100, UNIT=OUTPUT_UNIT) j
    IF (iStat <> 0) THEN
        WRITE(0, *) "WRITE(OUTPUT_UNIT) <", iStat, ">"
        CALL zzrc( 9 )
    END IF

    WAIT(IOSTAT=iStat, UNIT=6)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT(6) <", iStat, ">"
        CALL zzrc( 10 )
    END IF


    !
    !  UNIT= Specifier Value is:
    !  c)  ERROR_UNIT (Unit=0)
    !
    WRITE(0, IOSTAT=iStat, FMT=100) i
    IF (iStat <> 0) THEN
        WRITE(0, *) "WRITE(0) <", iStat, ">"
        CALL zzrc( 11 )
    END IF

    WAIT(ERROR_UNIT, IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT(ERROR_UNIT) <", iStat, ">"
        CALL zzrc( 12 )
    END IF


    WRITE(FMT=100, IOSTAT=iStat, UNIT=ERROR_UNIT) j
    IF (iStat <> 0) THEN
        WRITE(0, *) "WRITE(ERROR_UNIT) <", iStat, ">"
        CALL zzrc( 13 )
    END IF

    WAIT(IOSTAT=iStat, UNIT=0)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT(0) <", iStat, ">"
        CALL zzrc( 14 )
    END IF


100 FORMAT(I2)

END PROGRAM unitSpecFileUnitNum04
