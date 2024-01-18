!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/23/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Read in comma mode and write out the same data
!                               out in POINT mode; test sequential access mode.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program commaEdit003
    complex cx(5)
    real(8) d1(10)
    real(4) r1
    real(16) q1

    common /blk1/ cx, d1, r1, q1

    r1 = 1.24
    q1 = qsqrt(r1*1.0q0)

    d1 = atan((/(i*1.0d0, i=1,10)/))

    cx = cmplx((/(d1(2*i-1), i=1,5)/), (/(d1(i*2), i=1,5)/), 4)

    open (10, file='test1', decimal='COMMA', round='UP')

    open (11, file='test2', form='formatted', sign='PLUS')

    write (10, *) cx
    write (10, *) d1, r1
    write (10, *) q1


    close (10)

    open (10, file='test1', decimal='COMMA')

    call convertMode(10, 11)
end

subroutine convertMode(unit_in, unit_out)
    integer, intent(in) :: unit_in, unit_out

    complex cx(5)
    real(8) d1(10)
    real(4) r1
    real(16) q1

    common /blk1/ cx, d1, r1, q1

    namelist /nml1/ cx, d1, r1, q1

    read (unit_in, *) cx
    read (unit_in, *) d1, r1
    read (unit_in, *) q1

    write(unit_out, nml=nml1)
end
