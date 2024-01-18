! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn023a2.f
! opt variations: -ql -qdefaultpv -qreuse=none

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn023a2.f
! %VERIFY: fpAssgn023a2.out:fpAssgn023a2.vf
! %STDIN:
! %STDOUT: fpAssgn023a2.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (deallocate the poly
!                               pointer will finalize the associated target)
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
        integer(k1)   :: id
    end type

    type, extends(base) :: child    ! (4)
        class (base(k1)), pointer :: data => null()

        contains

        final :: finalizeChild
    end type

    class (child(4)), pointer :: c1_m (:)
    type (child(4)), pointer :: c2_m (:)

    contains

    subroutine finalizeChild (d)
        type (child(4)), intent(inout) :: d (:)

        do i = 1, size (d)
            print *, 'checking data', i
            if (associated (d(i)%data)) then
                deallocate (d(i)%data)
            end if
        end do
    end subroutine

    subroutine allocateC2_m
        allocate (c2_m(2))
    end subroutine
end module

program fpAssgn023a2
use m
    class (base(4)), pointer :: b_ptr (:)
    class (child(4)), pointer :: c1 (:)

    allocate (c1(2:4))

    b_ptr => c1

    deallocate (b_ptr)

    allocate (c1_m(10))

    b_ptr => c1_m

    deallocate (b_ptr)

    call allocateC2_m

    c1 => c2_m
    b_ptr => c1

    deallocate (b_ptr)
end
