! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (poly-pointer dummy-arg
!*                               used in the type bound; arrays)
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
        procedure, non_overridable :: copyData => copyBaseData
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

    subroutine copyBaseData (b, bArray, arraySize)
        class (base), intent(in) :: b
        class (base), intent(out), pointer :: bArray(:)
        integer*4, intent(in) :: arraySize

        allocate (bArray (arraySize), source=b)
    end subroutine
end module

program fArg005a11
use m
    type (base) :: b1 = base (10)
    type (child) :: c1

    class (base), pointer :: b2(:), b3(:)

    c1%id = 20

    call b1%copyData (b2, 3)

    if (size (b2) /= 3) error stop 1_4

    do i = 1, 3
        call b2(i)%print
    end do


    call c1%copyData (b3, 2)

    if (size (b3) /= 2) error stop 2_4

    call b3(1)%print
    call b3(2)%print

    deallocate (b2, b3)
end
