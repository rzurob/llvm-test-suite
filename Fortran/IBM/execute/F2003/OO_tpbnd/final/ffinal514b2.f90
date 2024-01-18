! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (allocatable function return result
!                               to be finalized (auto-deallocated) after use;
!                               test the call statement)
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
        integer(4) :: id

        contains

        procedure, nopass :: makeBaseObj => produceBaseAlloc
        final :: finalizeBase, finalizeBaseRank1
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure, nopass :: makeChildObj => produceChildAlloc
        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    type (base) function produceBaseAlloc (id, l, u)
        allocatable produceBaseAlloc (:)

        integer(4), intent(in) :: id, l, u

        allocate (produceBaseAlloc(l:u))

        produceBaseAlloc%id = id
    end function

    type (child) function produceChildAlloc (id, name, l, u)
        allocatable produceChildAlloc (:)
        integer(4), intent(in) :: id, l, u
        character(*), intent(in) :: name

        allocate (produceChildAlloc(l:u))

        produceChildAlloc%id = id
        produceChildAlloc%name = name
    end function

    subroutine printBase (b)
        type (base), allocatable, intent(in) :: b(:)

        if (allocated (b)) then
            print *, lbound(b,1), ubound(b,1)

            do i = lbound(b,1), ubound(b,1)
                print *, b(i)%id
            end do
        end if
    end subroutine

    subroutine printChild (c)
        type (child), allocatable, intent(in) :: c(:)

        if (allocated (c)) then
            print *, lbound(c,1), ubound(c,1)

            do i = lbound(c,1), ubound(c,1)
                print *, c(i)%id, c(i)%name
            end do
        end if
    end subroutine

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child), intent(in) :: c(:)

        print *, 'finalizeChildRank1'
    end subroutine
end module

program ffinal514b2
use m
    type (child) c1

    call printChild (c1%makeChildObj (1, 'c1', 2, 3))

    call printBase (c1%makeBaseObj (10, 0, 2))

    print *, 'end'
end
