!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc008a1.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 07/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (variable used in bounds info in
!                               allocate is computed)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

!! module m contains taylor series info for 1/(1-x)
module m
    real(8), allocatable :: ts(:)
    integer(4) :: tSize

    contains

    !! this subroutine sets the minmum size for taylor series based on the value
    !of x (d), and torlerance (e): the series is for 1/(1-x) where -1<x<1; more
    !elements in the series will not improve the accuracy for the level given
    !by e
    subroutine computeSize (d, e)
        real(8), intent(in) :: d, e

        tSize = int(log(e)/log(d), 4) + 1
    end subroutine
end module

program falloc008a1
use m
    real(8) :: x, error

    !! set x and error to a level that requires a long series to converge
    x = .9d0
    error = 1.d-5

    call computeSize (x, error)

    call generateTS (x)

    if (.not. precision_real8 (sum(ts), 1.d0/(1.d0-x), 2.d0*error)) error stop 1_4

    !! set x and error to a more difficult level to converge
    x = .99d0
    error = 1.d-7

    call computeSize (x, error)

    call generateTS (x)

    if (.not. precision_real8 (sum(ts), 1.d0/(1.d0-x), 2.d0*error)) error stop 2_4


    !! let's take an easy one
    x = .1d-3
    error = 1.d-9

    call computeSize (x, error)

    call generateTS (x)

    if (.not. precision_real8 (sum(ts), 1.d0/(1.d0-x), 2.d0*error)) error stop 3_4

    contains

    !! to see if two args are within the required relative errors
    logical function precision_real8 (d1, d2, e)
        real(8), intent(in) :: d1, d2, e

        precision_real8 = (dabs(d1-d2)/(d1+d2) <= .5d0*e)
    end function
end


subroutine generateTS (x)
use m
    real(8), intent(in) :: x

    if (allocated(ts))    deallocate (ts)

    allocate (ts(tSize), source=(/(x**i, i = 0, tSize-1)/))
end subroutine
