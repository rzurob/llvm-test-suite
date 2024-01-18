!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffuncRet006a1.f
! %VERIFY: ffuncRet006a1.out:ffuncRet006a1.vf
! %STDIN:
! %STDOUT: ffuncRet006a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : function return (poly function return of
!                               rank-two allocatable array)
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

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    interface makeData
        class (base) function makeBaseAlloc (id, name, size1, size2)
            import base
            allocatable makeBaseAlloc(:,:)

            integer(4), intent(in) :: id, size1, size2
            character(*), intent(in), optional :: name
        end function
    end interface

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

program ffuncRet006a1
use m
    associate (x => makeData (10, size1=10, size2=3),&
               x1 => makeData (30, 'x1', 6, 5))

        if (any (shape(x) /= (/10, 3/))) error stop 1_4
        if (any (shape(x1) /= (/6, 5/))) error stop 2_4

        call x(1,1)%print
        call x1(1,1)%print

        call x(3,2)%print
        call x1(3,2)%print

        call x(5,3)%print
        call x1(5,3)%print
    end associate
end

class (base) function makeBaseAlloc (id, name, size1, size2)
use m, only: base, child
    allocatable makeBaseAlloc(:,:)

    integer(4), intent(in) :: id, size1, size2
    character(*), intent(in), optional :: name

    integer k

    if (present(name)) then
        allocate (makeBaseAlloc(size1, size2), source=child(id, name))
    else
        allocate (makeBaseAlloc(size1, size2), source=base(id))
    end if

    k = 0

    do j = 1, size2
        do i = 1, size1
            makeBaseAlloc(i, j)%id = id + k
            k = k + 1
        end do
    end do
end function
