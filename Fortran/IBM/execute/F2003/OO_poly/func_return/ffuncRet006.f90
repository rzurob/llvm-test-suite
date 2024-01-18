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
! %GROUP: ffuncRet006.f
! %VERIFY: ffuncRet006.out:ffuncRet006.vf
! %STDIN:
! %STDOUT: ffuncRet006.out
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
!*  DATE                       : 08/24/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : poly-func-return (poly-function return results
!                               in ASSOCIATE construct)
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
        class (base) function makeBaseAlloc (id)
            import base
            allocatable makeBaseAlloc

            integer(4), intent(in) :: id
        end function

        class (base) function makeChildAlloc (id, name)
            import base
            allocatable makeChildAlloc

            integer(4), intent(in) :: id
            character(*), intent(in) :: name
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

program ffuncRet006
use m
    associate (x => makeData (10), x1 => makeData (20, 'x1'))
        call x%print
        call x1%print
    end associate
end

class (base) function makeChildAlloc (id, name)
use m, only : base, child
    allocatable makeChildAlloc
    integer(4), intent(in) :: id
    character(*), intent(in) :: name

    allocate (makeChildAlloc, source=child(id, name))
end function

class (base) function makeBaseAlloc (id)
use m, only : base
    allocatable makeBaseAlloc
    integer(4), intent(in) :: id

    allocate (makeBaseAlloc, source=base(id))
end function
