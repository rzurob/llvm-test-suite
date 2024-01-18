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
!*    test different ROUND mode with different data edit descriptor with
!*    real*4. ROUND= specifier is specified in WRITE statement.
!* ===================================================================

  program roundR4WriteEdit03 

    implicit none
 
    character(18) :: r_mode(6), r_verify
    integer i
    real w1, w2, w3, w4

    integer, parameter::unit = 2 

    ! round in up mode

    open(unit, file='roundR4WriteEdit03.out', action='write')

    w1 = 1.250058651 
    w2 = -1.250058651
    w3 = 3.141592653589
    w4 = 2.718281828459 

    r_mode=(/"up               ", "down             ",           &
             "zero             ", "nearest          ",           &
             "processor_defined", "compatible       "/)

    do i =1,6

       write(unit, '(4x,f7.5, 1x, f8.5)', round=r_mode(i)) w1, w2 

       write(unit, '(f13.6, 1x, en13.6, 1x, es13.6, 1x, g13.5, 1x,  &
        & d13.5, 1x, e13.5)', round=r_mode(i)) w3, w4, w3, w4, w3, w4

       inquire(unit, round=r_verify)

       if(r_verify .ne. "PROCESSOR_DEFINED") then
          error stop 1_4 
       endif

    end do

    close(unit)

  end program roundR4WriteEdit03 
