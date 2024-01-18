!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn008a.f
! %VERIFY: fpAssgn008a.out:fpAssgn008a.vf
! %STDIN:
! %STDOUT: fpAssgn008a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (unlimited poly-pointer
!                               array as the LHS and is a structure component)
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
    end type

    type, extends(base) :: child
        integer*4 :: id
    end type
end module

module m1
use m
    type container
        type (container), pointer :: next => null()
        class(*), pointer :: data (:) => null()

        contains

        procedure :: print => printData
    end type

    contains

    subroutine printData (d)
        class (container), target, intent(in) :: d

        type (container), pointer :: iterator

        iterator => d

        do while (associated (iterator))
            if (associated (iterator%data)) then
                print *, 'shape of the data:', shape (iterator%data)
                print *, lbound(iterator%data,1), 'to', ubound(iterator%data,1)
            end if

            iterator => iterator%next
        end do
    end subroutine
end module

program fpAssgn008a
use m1
    integer (8), target :: i1 (2:5)
    class (base), allocatable, target :: b1 (:)
    type (child), target :: c1 (-1:1)

    class (*), pointer :: x(:)

    type (container) list, iterator
    pointer iterator

    allocate (child :: b1 (2:3))

    x => b1

    list%data => x

    !! the second node
    allocate (list%next)

    iterator => list%next

    iterator%data => i1

    !! the third node
    allocate (iterator%next)

    iterator => iterator%next

    iterator%data => c1

    call list%print
end
