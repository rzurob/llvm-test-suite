!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Do not
!                              specify proc-interface.
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

    contains

    function func1(i)
        integer, intent(in) :: i
        procedure(), pointer :: func1

        if(i .EQ. 1) then
            func1 => sub2
        else
            func1 => sub3
        endif
    end function

    subroutine sub2(i)
        integer, intent(in) :: i
        print *, "sub2", i
    end subroutine

    subroutine sub3(b, c)
        type(Base), intent(in) :: b
        type(Base), intent(in) :: c
        print *, "sub3", b, c
    end subroutine
end module

module n
use m
    implicit type(Base) (p)

    type Container
        procedure(), nopass, pointer :: pp1 => null()
    end type
end module

program functionReturn003
use n
    implicit type(Base) (p,f), type(Container) (c)

    if(associated(c1%pp1)) error stop 1_4

    c1%pp1 => func1(1)
    if(.NOT. associated(c1%pp1)) error stop 2_4
    call c1%pp1(5)

    nullify(c1%pp1)
    if(associated(c1%pp1)) error stop 3_4

    c1%pp1 => func1(2)
    if(.NOT. associated(c1%pp1)) error stop 4_4
    call c1%pp1(Base(6), Base(7))
end
