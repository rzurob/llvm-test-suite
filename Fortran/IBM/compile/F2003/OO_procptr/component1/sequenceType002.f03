!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Declare a procedure pointer component for
!                              sequence type. Procedure interface is
!                              specified using interface-name.
!
!                              This test case is diagnostic for
!                              associating procedure pointer with
!                              internal procedure. The actual procedure
!                              associated has different name from
!                              interface-name.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        sequence
        integer i
        integer j
    end type

    interface
        subroutine interfaceSub1(i, b)
        import Base
            integer, intent(in) :: i
            type(Base), intent(in) :: b
        end subroutine

        function interfaceFunc1(i, j)
        import Base
            integer, intent(in) :: i
            integer, intent(in) :: j
            type(Base) :: interfaceFunc1
        end function
    end interface

    type Seq
        sequence
        integer k
        procedure(interfaceSub1), nopass, pointer :: pp1
        type(Base) :: b
        procedure(interfaceFunc1), nopass, pointer :: pp2
    end type
end module

program sequenceType002
use m
    type(Seq) :: s1
    s1%k = 10
    s1%b = Base(20, 30)
    s1%pp1 => sub1
    s1%pp2 => func1

    contains

    subroutine sub1(i, b)
    use m, only : Base
        integer, intent(in) :: i
        type(Base), intent(in) :: b
        print *, "sub1", i, b
    end subroutine

    function func1(i, j)
    use m, only : Base
        integer, intent(in) :: i
        integer, intent(in) :: j
        type(Base) :: func1
        func1 = Base(i, j)
    end function
end
