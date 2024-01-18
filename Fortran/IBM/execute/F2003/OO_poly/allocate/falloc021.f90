!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc021.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (if function return result is pointer;
!                               it can be disassociated)
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

        procedure, nopass :: genPtr => genBasePtr
    end type

    contains

    class (base) function genBasePtr (id)
        integer(4), intent(in), optional :: id

        pointer genBasePtr

        if (present (id))  then
            allocate (genBasePtr, source= base(id))
        else
            nullify (genBasePtr)
        end if
    end function
end module

program falloc021
use m
    class(base), allocatable :: b1(:)
    class (base), pointer :: b1_ptr

    allocate (b1(-1:0))

    b1_ptr => b1%genPtr(10)

    if (b1_ptr%id /= 10) error stop 1_4

    if (associated (b1_ptr%genPtr()) .or. associated (b1%genPtr())) error stop 2_4

    deallocate (b1_ptr)
end
