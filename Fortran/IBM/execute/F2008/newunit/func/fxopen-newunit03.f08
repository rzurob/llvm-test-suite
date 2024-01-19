! *********************************************************************
!* ===================================================================
!*
!* DATE                         : Oct. 2010
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: NEWUNIT= specifier, Feature#:377344
!* SECONDARY FUNTIONS TESTED    : READ,WRITE,REWIND
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Testing differen integer data type
!*                                ( integer *1,*2,*4,*8) for NEWUNIT=value.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/10/21    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    PROGRAM FXOPEN_NEWUNIT03
    IMPLICIT none

    TYPE new
         INTEGER*1 i1_in
         INTEGER*2 i2_in
         INTEGER*4 i4_in
         INTEGER*8 i8_in
    END TYPE new

    TYPE(new)  IVAR
    INTEGER value_in(50),m,value_out(50)
    CHARACTER(LEN = 10) acc
    CHARACTER(LEN = 3) asyn
    CHARACTER(LEN = 7) stat

    DO m = 1, 50
    value_in(m)=m
    END DO

    OPEN(NEWUNIT=IVAR%i1_in, FILE='fxopen_i1.dat',ACCESS='sequential', ASYNCHRONOUS='yes', STATUS='replace')

    DO m = 1, 50
    WRITE(IVAR%i1_in, *) value_in(m)
    END DO

    REWIND(IVAR%i1_in)

    DO m = 1, 50
    READ(IVAR%i1_in, *) value_out(m)
    END DO

    DO m = 1, 50
     if ( value_in(m) .ne. value_out(m) ) error stop 1_4
    END DO

    if ( IVAR%i1_in > -2 ) then
    print *, IVAR%i1_in
    error stop 2_4
    end if

    OPEN(NEWUNIT=IVAR%i2_in, FILE='fxopen_i2.dat',ACCESS='sequential', ASYNCHRONOUS='yes', STATUS='replace')

    DO m = 1, 50
    WRITE(IVAR%i2_in, *) value_in(m)
    END DO

    REWIND(IVAR%i2_in)

    DO m = 1, 50
    READ(IVAR%i2_in, *) value_out(m)
    END DO

    DO m = 1, 50
     if ( value_in(m) .ne. value_out(m) ) error stop 10_4
    END DO

    if ( IVAR%i2_in > -2 ) then
    print *, IVAR%i2_in
    error stop 20_4
    end if

    OPEN(NEWUNIT=IVAR%i4_in, FILE='fxopen_i4.dat',ACCESS='sequential', ASYNCHRONOUS='yes', STATUS='replace')

    DO m = 1, 50
    WRITE(IVAR%i4_in, *) value_in(m)
    END DO

    REWIND(IVAR%i4_in)

    DO m = 1, 50
    READ(IVAR%i4_in, *) value_out(m)
    END DO

    DO m = 1, 50
     if ( value_in(m) .ne. value_out(m) ) error stop 30_4
    END DO

    if ( IVAR%i4_in > -2 ) then
    print *, IVAR%i4_in
    error stop 40_4
    end if

    OPEN(NEWUNIT=IVAR%i8_in, FILE='fxopen_i8.dat',ACCESS='sequential', ASYNCHRONOUS='yes', STATUS='replace')

    DO m = 1, 50
    WRITE(IVAR%i8_in, *) value_in(m)
    END DO

    REWIND(IVAR%i8_in)

    DO m = 1, 50
    READ(IVAR%i8_in, *) value_out(m)
    END DO

    DO m = 1, 50
     if ( value_in(m) .ne. value_out(m) ) error stop 50_4
    END DO

    if ( IVAR%i8_in > -2 )  then
    print *, IVAR%i8_in
    error stop 60_4
    end if


    CLOSE(IVAR%i1_in)
    CLOSE(IVAR%i2_in)
    CLOSE(IVAR%i4_in)
    CLOSE(IVAR%i8_in)

    END PROGRAM FXOPEN_NEWUNIT03
