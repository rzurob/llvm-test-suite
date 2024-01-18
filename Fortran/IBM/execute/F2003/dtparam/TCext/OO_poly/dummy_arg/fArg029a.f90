! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg029a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (function return results
!                               used as the actual-arg)
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
        procedure :: replicate => replicateBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name ='default'

        contains

        procedure :: print => printChild
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

    class (base(4)) function replicateBase (b)
        class (base(4)), intent(in) :: b
        pointer replicateBase

        allocate (replicateBase, source=b)
    end function


    subroutine printData (b)
        class (base(4)), intent(in) :: b

        call b%print
    end subroutine
end module

program fArg029a
use m
    class (base(4)), allocatable :: b1

    type (child(4,1,20)) :: c1 = child(4,1,20) (2, 'c1')

    allocate (b1, source=child(4,1,20)(3, 'b1'))

    !! the following 2 calls leak memory
    call printData (c1%replicate())

    call printData (b1%replicate())

end
