! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet005.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
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
    type, ABSTRACT :: base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure (printBase), pass (b), deferred :: print
    end type

    type, extends (base) :: child    ! (4,20)
        integer(k1) :: id

        contains

        procedure :: print => printChild
    end type

    interface
        subroutine printBase (b)
        import base
            class (base(4,*)), intent(in) :: b
        end subroutine
    end interface

    contains

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b

        print *, b%id
    end subroutine

    class (base(4,:)) function produceBasePtr (b)
        class (base(4,*)), intent(in) :: b
        pointer produceBasePtr

        allocate (produceBasePtr, source=b)
    end function
end module

program ffuncRet005
use m
    class (base(4,:)), pointer :: b

    b => produceBasePtr (child(4,20) (10))

    call b%print

    deallocate (b)
end