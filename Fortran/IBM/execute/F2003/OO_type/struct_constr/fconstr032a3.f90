!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr032a3.f
! %VERIFY: fconstr032a3.out:fconstr032a3.vf
! %STDIN:
! %STDOUT: fconstr032a3.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : STRUCTURE CONSTRUCTOR (polymorphic allocatable
!                               components in structure constructor)
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
        integer*4 :: id = 1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
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

module m1
use m
    type container
        class (base), allocatable :: data(:)
    end type
end module

program fconstr032a3
use m1
    type (container) :: co1

    class (base), allocatable :: b1(:)
    class (base), allocatable :: c1(:)

    allocate (b1(2:3), source = (/child(2,'test2'), child(3,'test3')/))

    co1 = container (b1)

    if (size (co1%data) /= 2) error stop 1_4

    if ((lbound(co1%data,1) /= 2) .or. (ubound (co1%data, 1) /= 3)) error stop 2_4

    call co1%data(2)%print
    call co1%data(3)%print

    co1 = container (c1)

    if (allocated (co1%data)) error stop 3_4

    deallocate (b1)

    co1 = container (b1)

    if (allocated (co1%data)) error stop 4_4
end
