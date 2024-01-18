! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qnodeferredlp -qreuse=self /tstdev/OO_poly/point_assgn/fpAssgn025a1.f
! opt variations: -qck -qnok -ql -qdefaultpv -qdeferredlp -qreuse=none

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
! %GROUP: fpAssgn025a1.f
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
!*  DESCRIPTION                : data pointer assignment (pointer and target can
!*                               be structure component; use rank-one arrays as
!*                               structure components)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = 0
    end type

    type, extends(base) :: child(n1)    ! (4,15)
        integer, len  :: n1
        character(n1) :: name = ''
    end type

    type (child(4,15)), target, save :: c1_m (2:11)
end module

module m1
use m
    type baseContainer(k2)    ! (4)
        integer, kind            :: k2
        class(base(k2)), pointer :: data(:) => null()
    end type

    type anyContainer(k3)    ! (4)
        integer, kind :: k3
        class (*), pointer :: data(:) => null()
    end type

end module

program fpAssgn025a1
use m1
    interface assignment (=)
        subroutine base2Unlimited (ac, bc)
        use m1
            type (anyContainer(4)), intent(out) :: ac
            type (baseContainer(4)), intent(in) :: bc
        end subroutine
    end interface
    type (anyContainer(4)) :: co_a1
    type (baseContainer(4)) :: co_b1

    class (child(4,15)), allocatable, target :: c1(:)

    co_b1 = baseContainer(4) (data = c1_m)

    co_a1 = co_b1

    if (.not. associated (co_a1%data, c1_m)) error stop 1_4

    if ((size (co_a1%data) /= 10) .or. (lbound(co_a1%data, 1) /= 2) .or. &
        (ubound(co_a1%data, 1) /= 11)) error stop 2_4

    allocate (c1(20))

    co_b1%data => c1
    co_a1%data => co_b1%data(::2)

    if (.not. associated (co_a1%data, c1(::2))) error stop 3_4

    if (size (co_a1%data) /= 10) error stop 4_4
end

subroutine base2Unlimited (ac, bc)
use m1
    type (anyContainer(4)), intent(out) :: ac
    type (baseContainer(4)), intent(in) :: bc

    ac%data => bc%data
end subroutine
