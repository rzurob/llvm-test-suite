!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffuncRet006a.f
! %VERIFY: ffuncRet006a.out:ffuncRet006a.vf
! %STDIN:
! %STDOUT: ffuncRet006a.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 12/13/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : function return (poly function return results;
!                               use rank-one array)
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
        class (base) function makeBaseAlloc (id, name, size)
            import base
            allocatable makeBaseAlloc(:)

            integer(4), intent(in) :: id, size
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

program ffuncRet006a
use m
    associate (x => makeData (10, size=10), x1 => makeData (30, 'x1', 6))
        if (any(shape(x) /= (/10/))) error stop 1_4
        if (any(shape (x1) /= (/6/))) error stop 2_4

        do i = 1, 5, 2
            call x(i)%print
            call x1(i)%print
        end do
    end associate
end

class (base) function makeBaseAlloc (id, name, size)
use m, only: base, child
    allocatable makeBaseAlloc(:)

    integer(4), intent(in) :: id, size
    character(*), intent(in), optional :: name

    if (present(name)) then
        allocate (makeBaseAlloc(size), source=child(id, name))
    else
        allocate (makeBaseAlloc(size), source=base(id))
    end if

    makeBaseAlloc%id = (/(i+id-1, i=1, size)/)
end function
