! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg005a15.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (pointer return from
!*                               function is used as actual argument for
!*                               poly-pointer dummy-arg)
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
        procedure, non_overridable :: replicate => replicateBase
        final  :: finalizeBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
        final  :: finalizeChild
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

    function replicateBase (b)
        class (base(4)), pointer :: replicateBase
        class (base(4)), intent(in) :: b

        allocate (replicateBase, source=b)
    end function

    subroutine abc (b)
        class (base(4)), pointer :: b

        if (associated (b)) then
            call b%print

            deallocate (b)
        end if
    end subroutine

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,1,*)), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine
end module

program fArg005a15
use m
    type (base(4)) :: b1
    type (child(4,1,20)) :: c1 = child(4,1,20)(2, 'c1')

    b1%id = 1

    call abc (b1%replicate())

    call abc (c1%replicate())
end
