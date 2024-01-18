! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg005a12.f
! SCCS ID Information
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = -1

        contains

        procedure :: print => printBase
        procedure, non_overridable :: copyData => copyBaseData
        procedure, non_overridable :: copyDataArray => copyBaseData2Array
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine copyBaseData (b, b1)
        class (base(4)), intent (in) :: b
        class (base(4)), allocatable, intent(out) :: b1

        allocate (b1, source=b)
    end subroutine

    subroutine copyBaseData2Array (b, b1, arraySize)
        class (base(4)), intent (in) :: b
        class (base(4)), allocatable, intent(out) :: b1 (:)
        integer*4, intent(in) :: arraySize

        allocate (b1(arraySize), source=b)
    end subroutine
end module

program fArg005a12
use m
    class (base(4)), allocatable :: b1, b2 (:), b3

    type (base(4)) :: b4
    type (child(4,1,20)) :: c1

    b4 = base(4) (10)

    c1 = child(4,1,20) (20, 'c1_stack_obj')

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
