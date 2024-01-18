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
!*  PRIMARY FUNCTIONS TESTED   : ROUND with different control and
!*                               data edit descriptor.
!*
!*  DESCRIPTION                : 
!*    test different ROUND mode interacting with other specifiers and/or
!*    control edit descriptors. 
!*    9.4.1  The modes of a connection to an external file may be changed
!*           by a subsequent OPEN statement that modifies the connection.
!* ===================================================================

  program roundR4WriteEdit04 

    implicit none
 
    character(18) :: r_mode 
    real*4 w1, w2, w3, w4

    integer, parameter::unit = 2 

    ! round in up mode

    open(unit, file='roundR4WriteEdit04.out', action='write',      &
      & decimal="comma", round="up  ", SIGN="plus")

    w1 = 1.250058651 
    w2 = -1.250058651
    w3 = 3.141592653589
    w4 = 2.718281828459 

    write(unit, '(4x,f8.5, 1x, f8.5)') w1, w2 

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'UP') then
       error stop 1_4
    endif
 
    write(unit, '(f13.6, 1x, en13.6, 1x, es13.6, 1x, g13.5, 1x,   &
        & d13.5, 1x, e13.5)') w3, w4, w3, w4, w3, w4

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'UP') then
       error stop 2_4 
    endif

    ! round in down mode, trailing blanks for specifiers 

    open(unit, file='roundR4WriteEdit04.out', action='write',    &
        & round="DoWn ", decimal="point ", sign="suppress ")

    write(unit, '(4x,f7.5, 1x, f8.5)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'DOWN') then
       error stop 3_4
    endif

    write(unit, '(f13.6, 1x, en13.6, 1x, es13.6, 1x, g13.5, 1x,   &
        & d13.5, 1x, e13.5)') w3, w4, w3, w4, w3, w4

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'DOWN') then
       error stop 4_4
    endif

    ! round in zero mode

    open(unit, file='roundR4WriteEdit04.out', action='write')

    write(unit, '(4x,dc, ss, RZ, f7.5, 1x, dp, sp, RZ, f8.5)') w1, w2

    inquire(unit, round=r_mode)

    write(unit, '(f13.6, 1x, dc, s,RZ, en13.6, 1x, dp, sp, RZ, es13.6, &
      & 1x, dc, ss, RZ, g13.5, 1x, dp, sp, RZ, d13.5, 1x, dc, ss, RZ,  &
      & e13.5)') w3, w4, w3, w4, w3, w4

    ! round in nearest mode

    open(unit, file='roundR4WriteEdit04.out', action='write')

    write(unit, '(4x,f8.5, 1x, f8.5)', round="nearest", decimal="comma", &
      &  sign="plus") w1, w2

    write(unit, '(f13.6,1x,en13.6,1x,es13.6,1x,g13.5,1x,d13.5, 1x,       & 
      & e13.5)', round="nearest", decimal="comma", sign="plus") w3, w4,  &
      & w3, w4, w3, w4


    ! round in compatible mode

    open(unit, file='roundR4WriteEdit04.out', action='write',            &
       & round="compatible")

    write(unit, '(4x,f8.5, 1x, f8.5)',decimal="comma",sign="plus ") w1,  &
       & w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'COMPATIBLE') then
       error stop 9_4
    endif

    write(unit, '(dc, ss, f13.6, 1x, en13.6, 1x, es13.6, 1x, g13.5, 1x,  &
       & d13.5, 1x, e13.5)') w3, w4, w3, w4, w3, w4

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'COMPATIBLE') then
       error stop 10_4
    endif

    ! round in processor defined mode

    open(unit, file='roundR4WriteEdit04.out', action='write',            &
        & round="processor_defined", decimal="comma", sign="plus")

    write(unit, '(4x,f8.5, 1x, f8.5)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 11_4
    endif

    write(unit, '(f13.6, 1x, en13.6, 1x, es13.6, 1x, g13.5, 1x, d13.5, 1x, &
        & e13.5)') w3, w4, w3, w4, w3, w4

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 12_4
    endif

    close(unit)

  end program roundR4WriteEdit04 
