! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (non-poly dummy args'
!*                               association)
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
        integer*4 :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name = 'no-name'

        contains

        procedure :: print => printChild
    end type

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

program fArg005a5
use m
    type (base), pointer :: b1, b2 (:)
    type (child), pointer :: c1, c2 (:)

    allocate (b1, source = base(10))
    allocate (c1, source = child (20, 'c1_pointer'))

    call abc (b1, c1)

    call b1%print
    call c1%print

    deallocate (b1, c1)

    b2 => null()
    nullify (c2)

    call cba (b2, c2)

    if ((size (b2) /= 1) .or. (size (c2) /= 1)) error stop 1_4

    call b2(1)%print
    call c2(1)%print

    deallocate (b2, c2)

    allocate (b2 (2), source=base(15))
    allocate (c2(3), source=child(25, 'c2_array_of_3'))

    call cba (b2, c2)

    if ((size (b2) /= 2) .or. (size (c2) /= 3)) error stop 2_4

    call b2(1)%print
    call b2(2)%print

    call c2(1)%print
    call c2(2)%print
    call c2(3)%print

    deallocate (b2, c2)


    contains

    subroutine abc (b, c)
        type (base), pointer :: b
        type (child), pointer :: c

        if (associated (b)) deallocate (b)

        if (associated (c)) deallocate (c)

        allocate (b, c)
    end subroutine

    subroutine cba (b, c)
        type (base), pointer :: b (:)
        type (child), pointer :: c(:)

        integer*4 :: b_size, c_size

        b_size = 1
        c_size = 1

        if (associated (b)) then
            b_size = size(b)
            deallocate (b)
        end if

        if (associated (c)) then
            c_size = size(c)
            deallocate (c)
        end if

        allocate (b(b_size), c(c_size))
    end subroutine
end
