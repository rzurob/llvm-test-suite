! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/declaration1/target002.f
! opt variations: -qnol

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Associate a proc-target with a data
!                              pointer.
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
end module

program target002
use m
    interface
        subroutine sub1(i, b)
        use m
            integer, intent(in) :: i
            type(Base(*,4)), intent(in) :: b
        end subroutine

        integer function func1()
        end function
    end interface

    procedure(sub1), pointer :: pp1
    procedure(func1), pointer :: pp2
    integer, pointer :: i1

    pp1 => sub1
    pp2 => func1

    i1 => pp1
    i1 => pp2
end

subroutine sub1(i, b)
use m
    integer, intent(in) :: i
    type(Base(*,4)), intent(in) :: b
    print *, "sub1", i, b
end subroutine

integer function func1()
    func1 = 20
end function
