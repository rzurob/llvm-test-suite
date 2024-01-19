!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Do not specify proc-interface. The
!                              associated procedure is either an
!                              external function or an external
!                              subroutine. Non-poly. Intrinsic or
!                              derived type, scalar.
!
!                              Use default implicit typing to match
!                              the procedure pointer with the target.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer i
    end type

    type Container
        procedure(), nopass, pointer :: pp1
    end type
end module

program procInterface002a
use m
    interface
        subroutine sub1(i)
            integer, intent(in) :: i(2,3)
        end subroutine

        subroutine sub2(b)
        use m
            type(Base), intent(in) :: b(10)
        end subroutine
    end interface

    type(Container) :: c1

    c1%pp1 => sub1
    call c1%pp1(reshape((/(i,i=1,6)/),(/2,3/)))

    c1%pp1 => sub2
    call c1%pp1((/(Base(i),i=1,10)/))
end

subroutine sub1(i)
    integer, intent(in) :: i(2,3)
    print *, "sub1", i
end subroutine

subroutine sub2(b)
use m
    type(Base), intent(in) :: b(10)
    print *, "sub2", b
end subroutine
