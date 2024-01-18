! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg011a2.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg011a2.f
! %VERIFY: fArg011a2.out:fArg011a2.vf
! %STDIN:
! %STDOUT: fArg011a2.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (deferred-shape array
!                               dummy-arg used as selector in ASSOCIATE)
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
        integer(k1)      id

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

    type (child(4,1,20)) :: c1_m (2:5)

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine test1 (b)
        class (base(4)), allocatable :: b(:)

        if (allocated(b)) then
            associate (x => b)
                if ((lbound(x,1) /= lbound(b,1)) .or. &
                    (ubound(x,1) /= ubound(b,1))) error stop 1_4

                do i = lbound(x,1), ubound(x,1)
                    call x(i)%print
                end do
            end associate
        end if
    end subroutine
end module

program fArg011a2
use m
    class (base(4)), allocatable :: b1 (:)

    c1_m%id = (/2,3,4,5/)
    c1_m%name = (/'c1_m_2', 'c1_m_3', 'c1_m_4', 'c1_m_5'/)

    allocate (b1(lbound(c1_m,1):ubound(c1_m,1)), source=c1_m)

    call test1 (b1)
end
