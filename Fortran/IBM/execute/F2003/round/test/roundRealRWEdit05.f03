!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with READ/WRITE statement
!*
!*  DESCRIPTION                :
!*                 test different ROUND mode for list-directed formatting.
!* ===================================================================

  program roundRealRWEdit05

    implicit none

    integer i
    character(18) :: r_mode(6)
    real*8  r1(6), r2(6), r3(6), r4(6)

    integer, parameter::unit_r = 2
    integer, parameter::unit_w = 3

    open(unit_r, access="sequential", form="formatted",             &
        & file="roundRealRWEdit05.dat")

    open(unit_w, access="sequential", form="formatted",             &
        & file="roundRealRWEdit05.out")

    r_mode=(/"up               ", "down             ",            &
             "zero             ", "nearest          ",            &
             "processor_defined", "compatible       "/)

    do i =1, 6
     read(unit_r,*, round=r_mode(i)) r1(i), r2(i), r3(i), r4(i)
     rewind(unit_r)
    end do

    do i =1, 6
     write(unit_w,*, round=r_mode(i)) r1(i), r2(i), r3(i), r4(i)
    end do

    close(unit_r)
    close(unit_w)

  end program roundRealRWEdit05
