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
! %GROUP: ftpbnd502a.f
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
!*  DATE                       : 03/12/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : specific type bound (elemental subroutine
!*                               called by scalar, array and array section)
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

module m
    type base
        integer id
        contains

        procedure, pass :: assgn => assgnBase
        procedure, pass :: compare => compareID
    end type

    type (base) :: b1_m(10), b2_m

    contains

    elemental subroutine assgnBase (b, i)
        class (base), intent(inout) :: b
        integer, intent(in):: i
        b%id = i
    end subroutine

    !! this binding could be applied to array section subscripted with vector
    elemental subroutine compareID (b, i, r)
        class (base), intent(in) :: b
        integer*4, intent(in) :: i
        logical*4, intent(out) :: r

        r = (b%id == i)

    end subroutine
end module

program ftpbnd502a
use m

    type(base), allocatable :: b1(:)
    integer*4 :: aSect(3) = (/1,2,3/)
    logical*4 :: ret(3)

    allocate (b1(100))

    b1 = base(0)

    do i = 1, 10
        call b1_m(i)%assgn (i*10)
    end do

    call b2_m%assgn(-1)

    if (.not. all(b1_m%id == (/(i,i=10,100,10)/))) error stop 1_4

    if (b2_m%id /= -1) error stop 2_4

    call b1(::2)%assgn(10)
    call b1(2::2)%assgn(-10)

    do i = 1, 100, 2
        if (b1(i)%id /= 10) error stop 3_4
        if (b1(i+1)%id /= -10) error stop 4_4
    end do

    call b1(aSect)%compare (10, ret)

    if ((.not. ret(1)) .or. (ret(2)) .or. (.not. ret(3))) error stop 5_4
end
