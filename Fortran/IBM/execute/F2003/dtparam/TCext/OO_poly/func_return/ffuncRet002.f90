! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet002.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffuncRet002.f
! %VERIFY: ffuncRet002.out:ffuncRet002.vf
! %STDIN:
! %STDOUT: ffuncRet002.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : poly-function-return (poly-data's vft)
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
        final :: finalizeBase
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: print => printChild
        final :: finalizeChild
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

    subroutine finalizeChild (c)
        type (child(4,1,*)), intent(inout) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

    class (base(4)) function replicateBase (b)
        class (base(4)), intent(in) :: b
        pointer replicateBase

        allocate (replicateBase, source=b)
    end function
end module

program ffuncRet002
use m
    class (base(4)), pointer :: b1

    b1 => replicateBase (child(4,1,20) (1, 'b1'))

    call b1%print

    deallocate (b1)
end
