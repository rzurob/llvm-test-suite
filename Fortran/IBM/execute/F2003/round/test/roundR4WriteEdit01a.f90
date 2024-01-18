!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with different data edit descriptor.
!*
!*  DESCRIPTION                :
!*           test ROUND mode's interaction with  IEEE round mode during
!*           write.
!* ===================================================================

  program roundR4WriteEdit01a

    use ieee_arithmetic
    implicit none

    character(18) :: r_mode
    real w1, w2, w3, w4

    integer, parameter::unit = 2

    w1 = 1.250058651
    w2 = -1.250058651
    w3 = 3.141592653589
    w4 = 2.718281828459

    ! round in up mode

    open(unit, file='roundR4WriteEdit01a.out', action='write')

    call ieee_set_rounding_mode(ieee_up) ! round to up mode

    write(unit, '(4x,f7.5, 1x, f8.5)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 1_4
    endif

    write(unit, '(f13.6, 1x, en13.6, 1x, es13.6, 1x, g13.5, 1x, d13.5, &
    & 1x, e13.5)') w3, w4, w3, w4, w3, w4

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 2_4
    endif

    ! round in down mode

    open(unit, file='roundR4WriteEdit01a.out', action='write')

    call ieee_set_rounding_mode(ieee_down) ! ROUND mode changed to "down"

    write(unit, '(4x,f7.5, 1x, f8.5)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 3_4
    endif

    write(unit, '(f13.6, 1x, en13.6, 1x, es13.6, 1x, g13.5, 1x, d13.5, 1x, &
    & e13.5)') w3, w4, w3, w4, w3, w4

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 4_4
    endif

    ! round in zero mode

    open(unit, file='roundR4WriteEdit01a.out', action='write')

    call ieee_set_rounding_mode(ieee_to_zero)  !ROUND mode changed to zero

    write(unit, '(4x,f7.5, 1x, f8.5)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 5_4
    endif

    write(unit, '(f13.6, 1x, en13.6, 1x, es13.6, 1x, g13.5, 1x, d13.5, 1x, &
    & e13.5)') w3, w4, w3, w4, w3, w4

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 6_4
    endif

    ! round in nearest mode

    open(unit, file='roundR4WriteEdit01a.out', action='write')

    call ieee_set_rounding_mode(ieee_nearest)

    write(unit, '(4x,f7.5, 1x, f8.5)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 7_4
    endif

    write(unit, '(f13.6, 1x, en13.6, 1x, es13.6, 1x, g13.5, 1x, d13.5, 1x, &
    & e13.5)') w3, w4, w3, w4, w3, w4

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 8_4
    endif

    ! round in compatible mode

    open(unit, file='roundR4WriteEdit01a.out', action='write',          &
    round="compatible")

    call ieee_set_rounding_mode(ieee_down) ! this should not have any effect

    write(unit, '(4x,f7.5, 1x, f8.5)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'COMPATIBLE') then
       error stop 9_4
    endif

    write(unit, '(f13.6, 1x, en13.6, 1x, es13.6, 1x, g13.5, 1x, d13.5, 1x, &
    & e13.5)') w3, w4, w3, w4, w3, w4

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'COMPATIBLE') then
       error stop 10_4
    endif

    ! round specifier not specified.

    open(unit, file='roundR4WriteEdit01a.out', action='write')

    write(unit, '(4x,f7.5, 1x, f8.5)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'COMPATIBLE') then
       error stop 11_4
    endif

    write(unit, '(f13.6, 1x, en13.6, 1x, es13.6, 1x, g13.5, 1x, d13.5, 1x, &
    & e13.5)') w3, w4, w3, w4, w3, w4

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'COMPATIBLE') then
       error stop 12_4
    endif

    ! round specifier or descriptor is processor defined.

    open(unit, file='roundR4WriteEdit01a.out', action='write',           &
       & round="PROCESSOR_DEFINED")

    call ieee_set_rounding_mode(ieee_nearest)

    write(unit, '(4x,f7.5, 1x, f8.5)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 13_4
    endif

    write(unit, '(f13.6, 1x, en13.6, 1x, es13.6, 1x, g13.5, 1x, d13.5, 1x, &
    & e13.5)') w3, w4, w3, w4, w3, w4

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 14_4
    endif

    close(unit)

  end program roundR4WriteEdit01a
