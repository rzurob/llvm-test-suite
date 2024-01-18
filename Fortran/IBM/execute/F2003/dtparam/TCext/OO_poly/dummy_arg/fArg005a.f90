! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg005a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg005a.f
! %VERIFY: fArg005a.out:fArg005a.vf
! %STDIN:
! %STDOUT: fArg005a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (allocatable dummy-arg;
!                               the actual-arg shall be of the same
!                               characteristic)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child    ! (4,20)
        integer(k1) :: id
        contains

        procedure :: print => printChild
    end type

    type, extends (child) :: gen3(k2)    ! (4,20,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printGen3
    end type

    contains

    subroutine createBase (b, id, name)
        class (base(4,:)), allocatable, intent(out) :: b
        integer*4, intent(in) :: id
        character(*), intent(in), optional :: name

        if (present (name)) then
            allocate (b, source=gen3(4,20,1)(id = id, name=name))
        else
            allocate (b, source=child(4,20)(id))
        end if
    end subroutine

    subroutine printBase (b)
        class (base(4,*)), intent(in) :: b
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printGen3 (b)
        class (gen3(4,*,1)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fArg005a
use m
    class (base(4,:)), allocatable :: b1

    call createBase (b1, 1, 'gen3_alloc')

    call b1%print

    call createBase (b1, 10)

    call b1%print
end
