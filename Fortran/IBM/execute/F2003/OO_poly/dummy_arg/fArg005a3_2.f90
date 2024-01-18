! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (poly-pointer dummy-arg)
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
    type base
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    interface
        subroutine copyBase (b, b1)
        import base
            class (base), pointer, intent(out) :: b
            class (base), intent(in) :: b1
        end subroutine

        subroutine copyBaseArray (b, b1)
        import base
            class (base), pointer, intent(out) :: b(:)
            class (base), intent(in) :: b1 (:)
        end subroutine
    end interface

    class (base), pointer :: b1_m, b2_m(:)

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fArg005a3_2
use m
    class (base), pointer :: b1, b2(:)
    type (child) :: c1(3)
    type (base) :: b3(2)

    allocate (b1, source=child(1, 'b1_pointer'))

    call copyBase (b1_m, b1)

    call b1_m%print

    deallocate (b1, b1_m)

    allocate (b1)

    b1%id = 2

    call copyBase (b1_m, b1)

    call b1_m%print

    deallocate (b1, b1_m)

    c1%id = (/3,4,5/)
    c1%name = (/'c1_3', 'c1_4', 'c1_5'/)

    allocate (b2(3), source=c1)

    call copyBaseArray (b2_m, b2)

    if (size(b2_m) /= 3) error stop 1_4


    call b2_m(1)%print
    call b2_m(2)%print
    call b2_m(3)%print

    deallocate (b2, b2_m)

    call copyBaseArray (b2_m, c1(1:2)%base)

    if (size(b2_m) /= 2) error stop 2_4

    call b2_m(1)%print
    call b2_m(2)%print

    deallocate (b2_m)

    b3%id = (/6, 7/)

    call copyBaseArray (b2_m, b3)

    if (size(b2_m) /= 2) error stop 3_4

    call b2_m(1)%print
    call b2_m(2)%print

    deallocate (b2_m)
end


subroutine copyBase (b, b1)
use m, only : base
    class (base), pointer, intent(out) :: b
    class (base), intent(in) :: b1

    allocate (b, source=b1)
end subroutine


subroutine copyBaseArray (b, b1)
use m, only : base
    class (base), pointer, intent(out) :: b (:)
    class (base), intent(in) :: b1 (:)

    allocate (b(size(b1)), source=b1)
end subroutine
