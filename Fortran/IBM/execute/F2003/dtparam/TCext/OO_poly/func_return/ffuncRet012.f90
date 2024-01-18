! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet012.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/10/2005
!*
!*  DESCRIPTION                : poly function return (use-rename of the type)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1

        contains

        procedure(printBase), pass(b), deferred :: print
    end type

    type, extends(base) :: child(k2)    ! (4,20,1)
        integer, kind             :: k2
        integer(k1)                  id
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    interface
        subroutine printBase (b)
        import base
            class(base(4,*)), intent(in) :: b
        end subroutine
    end interface

    contains

    class(base(4,:)) function produceBasePtr (id, name)
        pointer produceBasePtr
        integer, intent(in) :: id
        character(*), intent(in) :: name

        allocate (produceBasePtr, source=child(4,20,1)(id,name))
    end function

    subroutine printChild (b)
        class(child(4,*,1)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program ffuncRet012
use m, absType => base
    class (absType(4,:)), pointer :: a1

    a1 => produceBasePtr (100, 'xlf test')

    call a1%print
end
