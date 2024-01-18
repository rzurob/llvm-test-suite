! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg026.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg026.f
! %VERIFY: fArg026.out:fArg026.vf
! %STDIN:
! %STDOUT: fArg026.out
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
!*  DESCRIPTION                : argument association (explicit interface is not
!                               required for some situations: dummy-arg is
!                               non-poly scalar)
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

program fArg026
use m
    type (base(4)) :: b1
    class (base(4)), allocatable :: b2
    type (child(4,1,20)), target :: c1 = child(4,1,20) (100, 'c1')

    class (base(4)), pointer :: b3

    allocate (b2, source = child(4,1,20)(1, 'b2'))

    b1 = base(4) (10)

    b3 => c1

    call abc (b1)

    call abc (b2)

    call abc (b3)
end

subroutine abc (b)
    use m
    type (base(4)), intent(in) :: b

    call b%print
end subroutine
