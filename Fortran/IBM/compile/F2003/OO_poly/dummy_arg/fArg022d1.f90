!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/07/2005
!*
!*  DESCRIPTION                : argument association (volatile attr.
!                               practically makes dummy-arg definable;
!                               undefinable actual-arg should be diagnosed; use
!                               the array section with vector subscript)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id
    end type

    interface
        subroutine externalAsyncCall (b)
        import base
            class (base), volatile :: b(:)
        end subroutine
    end interface
end module

program fArg022d1
use m
    type (base) :: b1 (10)

    call externalAsyncCall (b1((/1,2/)))
end
