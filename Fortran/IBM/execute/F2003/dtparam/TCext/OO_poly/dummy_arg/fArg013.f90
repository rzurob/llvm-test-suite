! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg013.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg013.f
! %VERIFY: fArg013.out:fArg013.vf
! %STDIN:
! %STDOUT: fArg013.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (TARGET attribute, no
!                               VALUE attribute; pointer association with
!                               dummy-arg before and after the procedure call)
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

    class (base(4)), pointer :: b_ptr, b_ptr2

    contains

    logical function isAssociated (b)
        class (base(4)), intent(inout), target :: b

        isAssociated = associated (b_ptr, b)

        if (isAssociated) then
            b_ptr%id = 1
            b_ptr2 => b
        end if
    end function

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine
end module

program fArg013
use m
    class (base(4)), pointer :: b1

    allocate (b1, source=child(4,1,20) (10, 'b1'))

    b_ptr => b1

    if (.not. isAssociated (b1)) error stop 1_4

    if (.not. associated (b_ptr, b_ptr2)) error stop 2_4

    if (.not. associated (b_ptr2, b1)) error stop 3_4

    call b1%print

    deallocate (b_ptr)
end
