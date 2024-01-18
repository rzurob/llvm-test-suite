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
!*    complex(16).
!*    9.4.1  The modes of a connection to an external file may be changed
!*           by a subsequent OPEN statement that modifies the connection.
!* ===================================================================

  program roundX16WriteEdit01 

    implicit none
 
    character(18) :: r_mode(6), r_verify
    integer i
    complex(16) w1, w2

    integer, parameter::unit = 2 

    open(unit, file='roundX16WriteEdit01.out', action='write')

    w1 = (1.2500586510356702302302302345691Q0,                         &
      & -1.2500586510356702302302302345691Q0)
    w2 = (3.14159265358959000230345532133343Q0,                        & 
      &  2.71828182845745223034878729912374Q0)

    r_mode=(/"up               ", "down             ",           &
             "zero             ", "nearest          ",           &
             "processor_defined", "compatible       "/)

    do i =1,6

       write(unit, '(1x,2f31.28, 1x, 2f25.23)', round=r_mode(i)) w1, w2

       write(unit, '(1x, 2en35.28, 1x, 2es35.23, 1x, 2g35.29, 1x, 2d35.24, &
        & 1x,2e35.29)', round=r_mode(i)) w1, w2, w1, w2, w1

       inquire(unit, round=r_verify)

       if(r_verify .ne. "PROCESSOR_DEFINED") then
          error stop 1_4
       endif

    end do

   close(unit)

  end program roundX16WriteEdit01 
