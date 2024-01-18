! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet007.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffuncRet007.f
! %VERIFY: ffuncRet007.out:ffuncRet007.vf
! %STDIN:
! %STDOUT: ffuncRet007.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : poly function return (allocatables will get
!                               deallocated after usage)
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

        final :: finalizeBase
        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

module m1
use m
    type (child(4,1,20)), save :: c1_m
    type (base(4)), save :: b1_m

    contains

    function makeData (b)
        class (base(4)), allocatable :: makeData
        class (base(4)), intent(in) :: b

        allocate (makeData, source=b)
    end function

    subroutine printData (d)
        class (base(4)), intent(in) :: d

        call d%print
    end subroutine

    subroutine printData1 (d)
        type (base(4)), intent(in) :: d

        call d%print
    end subroutine
end module

program ffuncRet007
use m1
    c1_m%id = 1
    c1_m%name = 'c1_m'

    call printData (makeData (c1_m))

    call printData1 (makeData (c1_m))

    print *, 'end'
end
