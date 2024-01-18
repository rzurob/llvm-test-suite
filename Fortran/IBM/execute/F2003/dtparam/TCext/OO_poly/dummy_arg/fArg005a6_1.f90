! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg005a6_1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited poly-pointer
!                               dummy-arg to be associated only with unlimited
!                               poly-pointer actual-arg)
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
    contains

    subroutine createObj (x, x1)
        class (*), pointer, intent(out) :: x
        class (*), intent(in) :: x1

        allocate (x, source=x1)
    end subroutine
end module

module m1
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id

        contains

        final :: finalizeBase
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,1,*)), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine
end module

program fArg005a6_1
use m
use m1
    class (*), pointer :: x => null()
    type (child(4,1,20)) :: c1

    call createObj (x, base(4)(10))

    if (.not. associated (x)) error stop 1_4

    print *, 'deallocating x'

    deallocate (x)

    call createObj (x, c1)

    deallocate (x)

    print *, 'end'
end
