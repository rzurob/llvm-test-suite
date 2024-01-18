! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg025a2.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg025a2.f
! %VERIFY: fArg025a2.out:fArg025a2.vf
! %STDIN:
! %STDOUT: fArg025a2.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (pointer/allocatable
!                               dummy-arg in the dummy-procedure and procedure
!                               declaration statement)
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

    interface
        subroutine alloc2Ptr (b1, b2)
        import base
            class (base(4)), intent(out), pointer :: b1(:)
            class (base(4)), intent(in), allocatable :: b2(:)
        end subroutine
    end interface

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module


program fArg025a2
use m
    class (base(4)) b1(:), b2(:)
    type (child(4,1,20)) :: c1 (3)

    procedure (alloc2Ptr) :: sub1

    pointer b1
    allocatable b2

    b1 => null()

    c1 = (/child(4,1,20)(1,'c1_1'), child(4,1,20)(2,'c1_2'),child(4,1,20)(3,'c1_3')/)

    allocate (b2 (3), source = c1)

    call sub1 (b1, b2)

    if (.not. associated (b1)) error stop 1_4

    if (size (b1) /= 3) error stop 2_4

    call b1(1)%print
    call b1(2)%print
    call b1(3)%print

    deallocate (b1)
end

subroutine sub1 (b1, b2)
use m
    class (base(4)), intent(out), pointer :: b1 (:)
    class (base(4)), intent(in), allocatable :: b2 (:)

    allocate (b1 (size(b2)), source=b2)
end subroutine
