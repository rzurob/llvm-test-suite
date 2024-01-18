!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg010a2.f
! %VERIFY: fArg010a2.out:fArg010a2.vf
! %STDIN:
! %STDOUT: fArg010a2.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (VALUE attribute; changes
!*                               made through pointer component will take
!*                               effect)
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

        procedure, non_overridable :: addID => addID2Base
    end type

    type, extends(base) :: child
        character*20 :: name
    end type

    type (base), target :: b1 = base (10)
    type (child), target :: c1 = child (20, 'c1')

    type (child), target :: c2 (2:4)
    type (base), target :: b2 (3:7)

    contains

    subroutine addID2Base (b, val)
        class (base), intent(inout) :: b
        integer*4, intent(in) :: val

        b%id = b%id + val
    end subroutine

    subroutine initializeB2C2
        b2 = (/(base(i), i=3,7)/)

        c2 = (/(child (i, 'c2'), i=2,4)/)
    end subroutine
end module

module m1
use m
    type container
        class (base), pointer :: data => null()
    end type

    type container1
        class (base), pointer :: data(:) => null()
    end type
end module

program fArg010a2
use m1
    type (container) :: co
    type (container1) :: co1

    class (base), allocatable, target :: b3, b4(:)


    call initializeB2C2

    call associateScalar (co, c1)

    call associateScalar (co, b1)

    call associateArray1 (co1, b2)

    call associateArray1 (co1, c2)


    ! validate that b1,b2,c1 and c2 are all modified

    if (b1%id /= 11) error stop 1_4

    if ((c1%id /= 21) .or. (c1%name /= 'c1')) error stop 2_4

    do i = 3, 7
        if (b2(i)%id /= i+2) error stop 3_4
    end do


    do i = 2, 4
        if (c2(i)%id /= i+2) error stop 4_4
    end do

    !! work with b3, b4

    allocate (b3, source = child (50, 'b3'))
    allocate (b4(10), source=base(100))


    call associateScalar (co, b3)

    call associateArray1 (co1, b4)

    if (b3%id /= 51) error stop 5_4

    if (any (b4%id /= 102)) error stop 6_4

    contains

    subroutine associateScalar (c, b)
        type (container), value :: c
        class (base), target, intent(inout) :: b

        c%data => b

        call c%data%addID (1)
    end subroutine

    subroutine associateArray1 (c, b)
        type (container1), value :: c
        class (base), target, intent(inout) :: b(:)

        c%data => b

        print *, lbound (c%data), ubound (c%data)

        do i = 1, size (c%data)
            call c%data(i)%addID (2)
        end do
    end subroutine
end
