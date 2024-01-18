! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_procptr/component2/target002.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Associate a proc-target with a data
!                              pointer. Poly and unlimited poly.
!
!                              This test case is diagnostic.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    interface
        subroutine sub1(b, c)
        !use m
        import Base
            class(Base(*,4)), intent(in) :: b
            class(*), intent(in) :: c
        end subroutine

        type(Base(20,4)) function func1(b, c)
        !use m
        import Base
            class(Base(*,4)), pointer :: b
            class(*), allocatable :: c
        end function
    end interface

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
        procedure(sub1), pointer, nopass :: pp1
        procedure(func1), pointer, nopass :: pp2
    end type
end module

program target002
use m
    type(Base(:,4)), pointer :: b1
    type(Child(20,4)) :: c1

    c1%pp1 => sub1
    c1%pp2 => func1

    b1 => c1%pp1
    b1 => c1%pp2
end

subroutine sub1(b, c)
use m, only : Base
    class(Base(*,4)), intent(in) :: b
    class(*), intent(in) :: c
    print *, "sub1", b%i
end subroutine

type(Base(20,4)) function func1(b, c)
use m, only : Base
    class(Base(*,4)), pointer :: b
    class(*), allocatable :: c
    func1 = Base(20,4)(b%i)
end function
