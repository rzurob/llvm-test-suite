!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg005a12.f
! %VERIFY: fArg005a12.out:fArg005a12.vf
! %STDIN:
! %STDOUT: fArg005a12.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (allocatable
!*                               poly-dummy-args' associations; use type-bound)
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
        procedure, non_overridable :: copyData => copyBaseData
        procedure, non_overridable :: copyDataArray => copyBaseData2Array
    end type

    type, extends (base) :: child
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

    subroutine copyBaseData (b, b1)
        class (base), intent (in) :: b
        class (base), allocatable, intent(out) :: b1

        allocate (b1, source=b)
    end subroutine

    subroutine copyBaseData2Array (b, b1, arraySize)
        class (base), intent (in) :: b
        class (base), allocatable, intent(out) :: b1 (:)
        integer*4, intent(in) :: arraySize

        allocate (b1(arraySize), source=b)
    end subroutine
end module

program fArg005a12
use m
    class (base), allocatable :: b1, b2 (:), b3

    type (base) :: b4
    type (child) :: c1

    b4 = base (10)

    c1 = child (20, 'c1_stack_obj')

    call c1%copyData (b1)

    call b1%print

    call b1%copyData (b3)

    call b3%print

    call b4%copyData (b1)

    call b1%print

    call c1%copyDataArray (b2, 2)

    if (size(b2) /= 2) error stop 1_4

    call b2(1)%print
    call b2(2)%print

    call b1%copyDataArray (b2, 3)

    if (size(b2) /= 3) error stop 2_4

    call b2(1)%print
    call b2(2)%print
    call b2(3)%print

end
