! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg004a4.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg004a4.f
! %VERIFY: fArg004a4.out:fArg004a4.vf
! %STDIN:
! %STDOUT: fArg004a4.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (nonpoly-explicit-shape
!                               arry dummy-arg used as actual-arg to be
!                               associated with poly-dummy-arg)
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
        procedure :: doubleID => doubleBaseID
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

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

    subroutine doubleBaseID (b)
        class (base(4)), intent(inout) :: b

        b%id = b%id*2
    end subroutine

    subroutine test1 (b)
        type (base(4)), intent(inout) :: b(3)

        do i = 1, 3, 2
            call b(i)%doubleID
        end do

        call test2 (b)
    end subroutine

    subroutine test2 (b)
        class (base(4)), intent(in) :: b(2:4)

        do i = 2, 4
            call b(i)%print
        end do
    end subroutine
end module

program fArg004a4
use m
    type (child(4,1,20)) :: c1(3)
    class (base(4)), pointer :: b1(:)

    c1 = (/child(4,1,20)(1,'c1_1'), child(4,1,20)(2,'c1_2'), child(4,1,20)(3,'c1_3')/)

    allocate (b1(3), source=c1)

    call test1 (b1)

    call test1 (c1%base)

    deallocate (b1)
end
