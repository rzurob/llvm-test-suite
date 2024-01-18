!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with different data edit descriptor.
!*
!*  DESCRIPTION                :
!*                   test ROUND= specifier with internal file during write.
!* ===================================================================

  program roundR8WriteEdit02

    implicit none

    character(18) :: r_mode(6), r_verify
    integer i
    real*8 w1, w2

    character(32) :: file_internal(6)

    character(32) :: file_v(6)

    file_v = (/'1.2500586510376 -1.2500586510355',              &
               '1.2500586510375 -1.2500586510356',              &
               '1.2500586510375 -1.2500586510355',              &
               '1.2500586510375 -1.2500586510356',              &
               '1.2500586510376 -1.2500586510356'/)

    w1 = 1.25005865103755D0
    w2 = -1.25005865103555D0

    r_mode=(/"up               ", "down             ",           &
             "zero             ", "nearest          ",           &
             "processor_defined", "compatible       "/)

    do i =1,6
       write(file_internal(i), '(f15.13, 1x, f16.13)', round=r_mode(i)) w1, w2
    end do

    do i=1,6
      if ( file_internal(i) .ne. file_v(i) ) call zzrc(i+1_4)
    end do

  end program roundR8WriteEdit02
