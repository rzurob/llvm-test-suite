!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 24/07/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND specifier with OPEN statment
!*
!*
!*  DESCRIPTION                : 
!*             test round= specifier with control edit descriptor "kP"
!*    9.5.4.13 test differnt ROUND= specifier in OPEN statement with WRITE
!*    9.4.1  The modes of a connection to an external file may be changed
!*           by a subsequent OPEN statement that modifies the connection.
!* ===================================================================

  program roundR4WriteEdit06 

    implicit none
 
    character(18) :: r_mode 
    real w1, w2, w3, w4

    integer, parameter::unit = 2 

    ! round= in up mode with "kp" control deit descriptor 

    open(unit, file='roundR4WriteEdit06.out', action='write', round="up",&
     & decimal="comma", sign="plus")

    w1 = 1234.2356 
    w2 = -1234.2356 
    w3 = 3.141592653589
    w4 = 2.718281828459 

    write(unit, '(4x,1pf9.2, 1x, 1pf9.2)') w1, w2 

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'UP') then
       error stop 1_4
    endif

    write(unit, '(1pf13.5, 1x, 2pen13.6, 1x, 1pes13.6, 1x, 2pg13.6, 1x, & 
      & 1pd13.6, 1x, 1pe13.6)') w3, w4, w3, w4, w3, w4

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'UP') then
       error stop 2_4 
    endif

    ! round DOWN with scale factor. r_mode is still "UP" 

    open(unit, file='roundR4WriteEdit06.out', action='write')

    write(unit, '(4x,RD, 1pf9.2, 1x, 1pf9.2)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'UP') then
       error stop 3_4
    endif

    write(unit, '(RD, 1pf13.5, 1x, 2pen13.6, 1x, 1pes13.6, 1x, 1pg13.6, &
      & 1x, 1pd13.6, 1x, 1pe13.6, RD)') w3, w4, w3, w4, w3, w4

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'UP') then
       error stop 4_4
    endif

    ! r_mode is still "up"

    open(unit, file='roundR4WriteEdit06.out', action='write')

    write(unit, '(4x, RU, 1pf9.2, 1x, RD, 1pf9.2)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'UP') then
       error stop 5_4
    endif

    write(unit, '(RU, 1pf13.5, 1x, RD, 2pen13.6, 1x, RZ,1pes13.6, 1x, RN, &
     & 2pg13.6, 1x, RC, 1pd13.6, 1x, RP, 1pe13.6)') w3, w4, w3, w4, w3, w4

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'UP') then
       error stop 6_4
    endif

    close(unit)

  end program roundR4WriteEdit06 
