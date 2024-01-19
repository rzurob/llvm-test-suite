!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with different data edit descriptor.
!*
!*  DESCRIPTION                :
!*    test different ROUND mode with different data edit descriptor with
!*    real*16 in WRITE statement.
!* ===================================================================

  program roundR16WriteEdit01

    implicit none

    character(18) :: r_mode(6), r_verify
    integer i
    real*16 w1, w2, w3, w4

    integer, parameter::unit = 2

    open(unit, file='roundR16WriteEdit01.out', action='write')

    w1 = 1.2500586510356702302302302375591Q0
    w2 = -1.2500586510356702302302302375591Q0
    w3 = 3.14159265358959000230345532133343Q0
    w4 = 2.71828182845745223034878729912374Q0

    r_mode=(/"up               ", "down             ",           &
             "zero             ", "nearest          ",           &
             "processor_defined", "compatible       "/)

    do i =1,6

       write(unit, '(4x,f30.28, 1x, f31.28)', round=r_mode(i)) w1, w2

       write(unit, '(f25.23, 1x, en32.23, 1x, es32.23, 1x, g32.24, 1x,  &
        & d32.24, 1x, e32.24)', round=r_mode(i)) w3, w4, w3, w4, w3, w4

       inquire(unit, round=r_verify)

       if(r_verify .ne. "PROCESSOR_DEFINED") then
          error stop 1_4
       endif

    end do

    close(unit)

  end program roundR16WriteEdit01
