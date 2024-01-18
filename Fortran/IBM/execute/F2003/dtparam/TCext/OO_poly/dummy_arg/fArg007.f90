! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg007.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg007.f
! %VERIFY: fArg007.out:fArg007.vf
! %STDIN:
! %STDOUT: fArg007.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (actual argument is a
!*                               pointer and dummy-arg is not; the association
!*                               is the pointer's target)
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
        integer(k1)   :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

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

    subroutine assignID (b, i)
        type (base(4)), intent(out) :: b
        integer*4, intent(in) :: i

        b%id = i
    end subroutine

    subroutine assignID2 (b, i)
        class (base(4)), intent(out) :: b
        integer*4, intent(in) :: i

        call assignID (b, i)
    end subroutine
end module

program fArg007
use m
    class (base(4)), pointer :: b1

    allocate (b1, source=child(4,1,20)(1, 'b1_pointer'))

    call assignID (b1, 10)

    call b1%print

    call assignID2 (b1, 20)

    call b1%print
end
