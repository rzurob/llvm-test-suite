! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (poly-allocatable array
!*                               dummy-arg associated)
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

    type, extends(base) :: child
        character*20 :: name

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

program fArg005a1
use m
    interface
        subroutine createBaseArray (b, id, name, arraySize)
        use m
            class (base), allocatable, intent(out) :: b(:)
            integer*4, intent(in) :: id, arraySize
            character(*), intent(in), optional :: name
        end subroutine
    end interface

    class (base), allocatable :: b1(:)

    call createBaseArray (b1, 1, 'b1_array', 3)

    if (size(b1) /= 3) error stop 1_4

    call b1(1)%print
    call b1(2)%print
    call b1(3)%print

    call createBaseArray (b1, 10, arraySize=2)

    if (size (b1) /= 2) error stop 2_4

    call b1(1)%print
    call b1(2)%print
end


subroutine createBaseArray (b, id, name, arraySize)
use m
    class (base), allocatable, intent(out) :: b(:)
    integer*4, intent(in) :: id, arraySize
    character(*), intent(in), optional :: name


    if (present(name)) then
        allocate (b(arraySize), source=child(id, name))
    else
        allocate (b(arraySize))
        b%id = id
    end if
end subroutine

