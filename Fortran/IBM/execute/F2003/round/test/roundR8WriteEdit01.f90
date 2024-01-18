!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with different data edit descriptor.
!*
!*  DESCRIPTION                :
!*    test different ROUND mode with different data edit descriptor with
!*    real*8. ROUND= specifier is specified in WRITE statement. Results
!*    is different for different mode.
!* ===================================================================

  program roundR8WriteEdit01

    implicit none

    character(18) :: r_mode(6), r_verify
    integer i
    real*8 w1, w2, w3, w4

    integer, parameter::unit = 2

    open(unit, file='roundR8WriteEdit01.out', action='write')

    w1 = 1.25005865103755D0
    w2 = -1.25005865103555D0
    w3 = 3.14159265358955D0
    w4 = 2.71828182845755D0

    r_mode=(/"up               ", "down             ",           &
             "zero             ", "nearest          ",           &
             "processor_defined", "compatible       "/)

    do i =1,6

       write(unit, '(31A,20A)') "rounding mode for real8 now is " ,r_mode(i)

       write(unit, '(4x,f15.13, 1x, f16.13)', round=r_mode(i)) w1, w2

       write(unit, '(f15.13, 1x, en22.13, 1x, es22.13, 1x, g22.14, 1x,  &
        & d22.14, 1x, e22.14)', round=r_mode(i)) w3, w4, w3, w4, w3, w4

       inquire(unit, round=r_verify)

       if(r_verify .ne. "PROCESSOR_DEFINED") then
          error stop 1_4
       endif

    end do

    close(unit)

  end program roundR8WriteEdit01
