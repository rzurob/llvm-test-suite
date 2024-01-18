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
!*                  C928: if a ... or round= specifier appears, a format 
!                   or namelist group name should also appear.
!* ===================================================================

  program roundR16WriteEdit02 

    implicit none
 
    character(18) :: r_mode(6), r_verify
    integer i
    real*16 w1, w2, w3, w4
    namelist /nmlist/ w1, w2, w3, w4

    integer, parameter::unit = 2 

    open(unit, file='roundR16WriteEdit02.out', action='write')

    w1 = 1.2500586510356702302302302375591Q0
    w2 = -1.2500586510356702302302302375591Q0
    w3 = 3.14159265358959000230345532133343Q0
    w4 = 2.71828182845745223034878729912374Q0

    r_mode=(/"up               ", "down             ",           &
             "zero             ", "nearest          ",           &
             "processor_defined", "compatible       "/)

    do i =1,6

       write(unit, nml=nmlist, round=r_mode(i)) 

    end do

    close(unit)

  end program roundR16WriteEdit02 
