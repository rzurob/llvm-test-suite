! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc008a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc008a.f
! %VERIFY: falloc008a.out:falloc008a.vf
! %STDIN:
! %STDOUT: falloc008a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (subsequent redefinition of lower and
!                               upper bounds values should not affect the array
!                               allocation)
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
    type base(k1,n1)    ! (1,15)
        integer, kind             :: k1
        integer, len              :: n1
        character(kind=k1,len=n1) :: name

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2)    ! (1,15,4)
        integer, kind :: k2
        integer(k2)   :: id

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(1,*)), intent(in) :: b

        print *, b%name
    end subroutine

    subroutine printChild (b)
        class (child(1,*,4)), intent(in) :: b

        print *, b%name, b%id
    end subroutine

    integer(4) function allocateBasePtr (b, lb, ub, source)
        class (base(1,*)), pointer, intent(out) :: b(:)
        class (base(1,*)), intent(in) :: source
        integer(4), intent(inout) :: lb, ub

        integer(4) error

        error = 1

        allocate (b(lb:ub), source=source, stat=error)

        lb = 1
        ub = 0
        allocateBasePtr = error
    end function
end module

program falloc008a
use m
    class (base(1,15)), pointer :: b1(:)
    integer(4) :: lb, ub

    lb = 0
    ub = 2

    if (allocateBasePtr (b1, lb, ub, child(1,15,4)('array_of_3', id = 3)) /= 0)  &
            error stop 1_4


    call b1(0)%print
    call b1(1)%print
    call b1(2)%print
end
