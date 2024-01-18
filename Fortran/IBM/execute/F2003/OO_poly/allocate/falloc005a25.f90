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
! %GROUP: falloc005a25.f
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
!*  DESCRIPTION                : ALLOCATE (sum() and array constructors used as
!                               source-expr; use taylor series for ln(1+x) where
!                               x is less than 1; choose this series as it
!                               converge fast)
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

program falloc005a25
    real(8), allocatable :: taylorSeries(:)
    real(8), pointer :: result
    real(8) x
    integer(4) :: tSize  ! series size

    !! set x and tSize at slow converge level
    x = 0.9d0
    tSize = 1000     !<-- this will be used to test SMP

    call generate (taylorSeries, tSize, x)

    allocate (result, source=sum (taylorSeries))

    if (.not. verifyResult (result, log(1.0d0+x), 2.d-14)) error stop 1_4

    deallocate (result)

    !! set x and tSize at a faster converge level
    x = .5d0
    tSize = 500      !<-- this will be used to test SMP

    call generate (taylorSeries, tSize, x)

    allocate (result, source=sum (taylorSeries))

    if (.not. verifyResult (result, log(1.0d0+x), 2.d-14)) error stop 2_4

    deallocate (result)


    !! now we set x and tSize to a level that the sum can be used comparing
    !directly to x
    x = 1.d-5
    tSize = 200   !<-- we overshoot the size to see if any wrongdoings by -qsmp

    call generate (taylorSeries, tSize, x)

    allocate (result, source=sum (taylorSeries))

    if (.not. verifyResult (result, x, 2.d0*dabs(x-log(1.d0+x))/x)) error stop 3_4

    deallocate (result)

    contains

    !! this subroutine generates a logrithm series for ln(1+x) at 0< x <1
    !! n is the size of the series and delat is what x is
    subroutine generate (ts, n, delta)
        real(8), allocatable, intent(out) :: ts(:)
        integer(4), intent(in) :: n
        real(8), intent(in) :: delta

        allocate (ts(n), source=(/((-1)**(i+1)*delta**i/real(i,8), i=1,n)/))
    end subroutine


    !! tests to see if the relative error between d1 and d2 are within ep
    logical function verifyResult (d1, d2, ep)
        real(8), intent(in) :: d1, d2, ep

        verifyResult = (dabs(d1-d2)/(d1+d2) <= .5d0*ep)
    end function
end
