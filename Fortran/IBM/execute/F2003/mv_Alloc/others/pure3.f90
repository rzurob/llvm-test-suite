! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/06/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic,
!*                               dummy args
!*                               move_alloc is called inside pure subroutine
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

    type shape
        contains
            final :: f1
            final :: f1_1
    end type

    type, extends(shape) :: point
        integer x
        integer y
        contains
           final :: f2
    end type

    class(*), allocatable :: z1(:,:)

    contains

        pure subroutine f1(x)
           type(shape), intent(in) :: x(:,:)
        end subroutine

        pure subroutine f1_1(x)
           type(shape), intent(in) :: x
        end subroutine

        pure subroutine f2(x)
           type(point), intent(in) :: x(:,:)
        end subroutine

end module

program main
use m

    class(*), allocatable :: x1(:,:), y1(:,:)
    integer i

    allocate(x1(2,2), source = reshape((/ (point(4-i,i+4),i=-2,4,2) /), (/2,2/)))

    allocate(y1(1,2), source = reshape((/(shape(), i=1,2)/),(/1,2/)))

    call move_alloc (x1, y1)

    call move_alloc (y1, z1)

    if ( allocated(x1) ) stop 21
    if ( allocated(y1) ) stop 23
    if ( .not. allocated(z1) ) stop 25

    select type(z1)
        type is (point)
            print *, z1%x
            print *, z1%y
        class default
            stop 33
    end select

end
