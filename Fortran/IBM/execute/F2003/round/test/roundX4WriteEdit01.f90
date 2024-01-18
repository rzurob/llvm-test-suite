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
!*  PRIMARY FUNCTIONS TESTED   : ROUND with different data edit descriptor.
!*                             
!*
!*  DESCRIPTION                : 
!*    test different ROUND mode using different data edit descriptor with
!*    complex(4).
!*    9.4.1  The modes of a connection to an external file may be changed
!*           by a subsequent OPEN statement that modifies the connection.
!* ===================================================================

  program roundX4WriteEdit01 

    implicit none
 
    character(18) :: r_mode 
    complex w1, w2

    integer, parameter::unit = 2 

    ! round in up mode

    open(unit, file='roundX4WriteEdit01.out', action='write', round="up")

    w1 = (1.250058651, -1.250058651)
    w2 = (3.141592653589, 2.718281828459)

    write(unit, '(1x,2f10.5, 1x, 2f15.6)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'UP') then
       error stop 1_4
    endif
 
    write(unit, '(1x, 2en13.5, 1x, 2es15.6, 1x, 2g13.6, 1x, 2d15.7, 1x, &
      & 2e14.6)') w1, w2, w1, w2, w1

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'UP') then
       error stop 2_4 
    endif

    ! round in down mode

    open(unit, file='roundX4WriteEdit01.out', action='write', round="down")

    write(unit, '(1x,2f10.5, 1x, 2f15.6)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'DOWN') then
       error stop 3_4
    endif

    write(unit, '(1x, 2en13.5, 1x, 2es15.6, 1x, 2g13.6, 1x, 2d15.7, 1x,  &
     & 2e14.6)') w1, w2, w1, w2, w1

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'DOWN') then
       error stop 4_4
    endif

    ! round in zero mode

    open(unit, file='roundX4WriteEdit01.out', action='write', round="zero")

    write(unit, '(1x,2f10.5, 1x, 2f15.6)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'ZERO') then
       error stop 5_4
    endif

    write(unit, '(1x, 2en13.5, 1x, 2es15.6, 1x, 2g13.6, 1x, 2d15.7, 1x, &
      & 2e14.6)') w1, w2, w1, w2, w1

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'ZERO') then
       error stop 6_4
    endif

    ! round in nearest mode

    open(unit, file='roundX4WriteEdit01.out', action='write', round="nearest")

    write(unit, '(1x,2f10.5, 1x, 2f15.6)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'NEAREST') then
       error stop 7_4
    endif

    write(unit, '(1x, 2en13.5, 1x, 2es15.6, 1x, 2g13.6, 1x, 2d15.7, 1x, &
      & 2e14.6)') w1, w2, w1, w2, w1

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'NEAREST') then
       error stop 8_4
    endif

    ! round in compatible mode

    open(unit, file='roundX4WriteEdit01.out', action='write',           &
      & round="compatible")

    write(unit, '(1x,2f10.5, 1x, 2f15.6)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'COMPATIBLE') then
       error stop 9_4
    endif

    write(unit, '(1x, 2en13.5, 1x, 2es15.6, 1x, 2g13.6, 1x, 2d15.7, 1x, &
       & 2e14.6)') w1, w2, w1, w2, w1

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'COMPATIBLE') then
       error stop 10_4
    endif

    ! round in processor defined mode

    open(unit, file='roundX4WriteEdit01.out', action='write',           &
       & round="processor_defined")

    write(unit, '(1x,2f10.5, 1x, 2f15.6)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 11_4
    endif

    write(unit, '(1x, 2en13.5, 1x, 2es15.6, 1x, 2g13.6, 1x, 2d15.7, 1x, &
       & 2e14.6)') w1, w2, w1, w2, w1

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 12_4
    endif

    ! round specifier or descriptor is not specified

    open(unit, file='roundX4WriteEdit01.out', action='write')

    write(unit, '(1x,2f10.5, 1x, 2f15.6)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 11_4
    endif

    write(unit, '(1x, 2en13.5, 1x, 2es15.6, 1x, 2g13.6, 1x, 2d15.7, 1x, &
      & 2e14.6)') w1, w2, w1, w2, w1

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 12_4
    endif

   close(unit)

  end program roundX4WriteEdit01 
