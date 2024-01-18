! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv /tstdev/OO_procptr/component1/sequenceType002.f
! opt variations: -ql -qdefaultpv -qreuse=self

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
    type Base(k1,k2)    ! (4,4)
        integer, kind :: k1,k2
        sequence
        integer(k1)      i
        integer(k2)      j
    end type

    interface
        subroutine interfaceSub1(i, b)
        import Base
            integer, intent(in) :: i
            type(Base(4,4)), intent(in) :: b
        end subroutine

        function interfaceFunc1(i, j)
        import Base
            integer, intent(in) :: i
            integer, intent(in) :: j
            type(Base(4,4)) :: interfaceFunc1
        end function
    end interface

    type Seq(k3)    ! (4)
        integer, kind     :: k3
        sequence
        integer(k3)          k
        procedure(interfaceSub1), nopass, pointer :: pp1
        type(Base(k3,k3)) :: b
        procedure(interfaceFunc1), nopass, pointer :: pp2
    end type
end module

program sequenceType002
use m
    type(Seq(4)) :: s1
    s1%k = 10
    s1%b = Base(4,4)(20, 30)
    s1%pp1 => sub1
    s1%pp2 => func1

    contains

    subroutine sub1(i, b)
    use m, only : Base
        integer, intent(in) :: i
        type(Base(4,4)), intent(in) :: b
        print *, "sub1", i, b
    end subroutine

    function func1(i, j)
    use m, only : Base
        integer, intent(in) :: i
        integer, intent(in) :: j
        type(Base(4,4)) :: func1
        func1 = Base(4,4)(i, j)
    end function
end
