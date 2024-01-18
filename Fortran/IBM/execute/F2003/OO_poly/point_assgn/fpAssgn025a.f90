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
! %GROUP: fpAssgn025a.f
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
!*  DATE                       : 03/26/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : data pointer assignment (pointer component;
!*                               mix of a few features; use container arrays)
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
        integer*4 :: id = 0
    end type

    type, extends(base) :: child
        character*15 :: name = ''
    end type

    type (child), target, save :: c1_m(10)
end module

module m1
use m
    type baseContainer
        class (base), pointer :: data => null()
    end type

    type anyContainer
        class (*), pointer :: data => null()
    end type

end module

program fpAssgn025a
use m1
    interface assignment (=)
        pure subroutine base2Unlimited (ac, bc)
        use m1
            class (anyContainer), intent(out) :: ac
            class (baseContainer), intent(in) :: bc
        end subroutine
    end interface

    type (anyContainer) :: co_a1(10)
    type (baseContainer) :: co_b1(10)

    character*1, target :: ch1(10)

    class (child), target, allocatable :: c1(:)

    allocate (c1(10))

    forall (i=1:10)
        co_a1(i)%data => ch1(i)
        co_b1(i)%data => c1_m(11-i)
    end forall

    do i = 1, 10
        if ((.not. associated (co_a1(i)%data, ch1(i))) .or. &
            (.not. associated (co_b1(i)%data, c1_m(11-i)))) error stop 1_4
    end do


    forall (i=1:10)
        co_a1(i) = co_b1(i)
    end forall

    do i = 1, 10
        if (.not. associated (co_a1(i)%data)) error stop 2_4
    end do
end

pure subroutine base2Unlimited (ac, bc)
use m1
    class (anyContainer), intent(out) :: ac
    class (baseContainer), intent(in) :: bc

    allocate (ac%data, source=bc%data)
end subroutine
