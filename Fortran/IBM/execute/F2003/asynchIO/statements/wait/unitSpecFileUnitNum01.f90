!*  ===================================================================
!*
!*  DATE                       : March  3, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement
!*  SECONDARY FUNCTIONS TESTED : UNIT= Specifier Value is a scalar-int-expr
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), UNIT= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 8
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

PROGRAM unitSpecFileUnitNum01

    OPEN(42, ACTION='readwrite', ASYNCHRONOUS='yes')


    WRITE(42, '(I3)', ASYNCHRONOUS='yes') 42
    WAIT( 42 )

    WRITE(42, '(I3)', ASYNCHRONOUS='yes') 42
    WAIT( UNIT=42 )


    REWIND 42


    READ(42, '(I3)', ASYNCHRONOUS='yes') i
    WAIT(25 + 25 - 8)

    READ(42, '(I3)', ASYNCHRONOUS='yes') j
    WAIT( UNIT=(20 + 20 + 2) )


    IF (i <> j) THEN
        PRINT *, "i = '", i, "', j = '", j, "'"
        CALL zzrc( 1 )

    ELSE IF (i <> 42) THEN
        PRINT *, "(i != 42)  i = '", i, "'"
        CALL zzrc( 2 )
    END IF


    WRITE(42, '(I3)', ASYNCHRONOUS='yes') j
    WAIT( i )

    WRITE(42, '(I3)', ASYNCHRONOUS='yes') j
    WAIT( UNIT=i )


    REWIND 42


    k = 21
    READ(42, '(I3)', ASYNCHRONOUS='yes') i
    WAIT( k + k )

    READ(42, '(I3)', ASYNCHRONOUS='yes') j
    WAIT( UNIT=((k * 3) - k) )


    CLOSE( 42 )


    IF (i <> j) THEN
        PRINT *, "i = '", i, "', j = '", j, "'"
        CALL zzrc( 3 )

    ELSE IF (i <> 42) THEN
        PRINT *, "(i != 42)  i = '", i, "'"
        CALL zzrc( 4 )
    END IF

END PROGRAM unitSpecFileUnitNum01
