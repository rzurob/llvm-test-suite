! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/component1/functionReturn001.f
! opt variations: -qnok -qnol

!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Specify
!                              proc-interface using interface-name.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    interface
        subroutine interfaceSub1(b)
        import Base
            type(Base(*,4)), intent(in) :: b
        end subroutine
    end interface
end module

module m2
use m1
    interface
        subroutine sub1(b)
        import Base
            type(Base(*,4)), intent(in) :: b
        end subroutine

        subroutine sub2(b)
        import Base
            type(Base(*,4)), intent(in) :: b
        end subroutine
    end interface
end module

module m3
    interface
        function func1(i)
        use m1
            integer, intent(in) :: i
            procedure(interfaceSub1), pointer :: func1
        end function
    end interface
end module

module m4
use m1
    type Container(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        procedure(interfaceSub1), nopass, pointer :: pp1
    end type
end module

program functionReturn001
use m1
use m3
use m4
    type(Container(4,20)) :: c1
    c1%pp1 => func1(1)
    call c1%pp1(Base(20,4)(5))

    c1%pp1 => func1(2)
    call c1%pp1(Base(20,4)(5))
end

subroutine sub1(b)
use m1
    type(Base(*,4)), intent(in) :: b
    print *, "sub1", b
end subroutine

subroutine sub2(b)
use m1
    type(Base(*,4)), intent(in) :: b
    print *, "sub2", b
end subroutine

function func1(i)
use m1
use m2
    integer, intent(in) :: i
    procedure(interfaceSub1), pointer :: func1

    if(i .EQ. 1) then
        func1 => sub1
    else
        func1 => sub2
    endif
end function
