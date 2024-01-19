! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg016a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (deferred-shape array's
!*                              bound can be by pointer assignment)
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

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    type (child(4,1,20)), target :: c1_m(3:5)

    contains

    subroutine reAssign (b)
        class (base(4)), pointer, intent(inout) :: b(:)

        if (associated (b)) then
            deallocate (b)
            b => c1_m
        end if
    end subroutine

    subroutine printChild (c)
        class (child(4,1,*)), intent(in) :: c

        print *, c%id, c%name
    end subroutine

    subroutine printBase (c)
        class (base(4)), intent(in) :: c

        print *, c%id
    end subroutine
end module

program fArg016a
use m
    class (base(4)), pointer :: b1 (:)

    allocate (b1 (10:20))

    if ((lbound (b1, 1) /= 10) .or. (ubound(b1, 1) /= 20)) error stop 1_4

    call reAssign (b1)

    if (.not. associated (b1, c1_m)) error stop 2_4

    if ((lbound (b1, 1) /=3) .or. (ubound (b1, 1) /= 5)) error stop 3_4

    c1_m%id = (/3,4,5/)
    c1_m%name = (/'c1_m_3', 'c1_m_4', 'c1_m_5'/)

    call b1(3)%print
    call b1(4)%print
    call b1(5)%print
end
