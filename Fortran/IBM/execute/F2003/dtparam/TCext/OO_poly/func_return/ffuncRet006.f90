! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet006.f
! SCCS ID Information
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
!*
!*  DATE                       : 08/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    interface makeData
        class (base(4)) function makeBaseAlloc (id)
            import base
            allocatable makeBaseAlloc

            integer(4), intent(in) :: id
        end function

        class (base(4)) function makeChildAlloc (id, name)
            import base
            allocatable makeChildAlloc

            integer(4), intent(in) :: id
            character(*), intent(in) :: name
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

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

class (base(4)) function makeChildAlloc (id, name)
use m, only : base, child
    allocatable makeChildAlloc
    integer(4), intent(in) :: id
    character(*), intent(in) :: name

    allocate (makeChildAlloc, source=child(4,1,20)(id, name))
end function

class (base(4)) function makeBaseAlloc (id)
use m, only : base
    allocatable makeBaseAlloc
    integer(4), intent(in) :: id

    allocate (makeBaseAlloc, source=base(4)(id))
end function
