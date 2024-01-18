! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg005a7_1.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg005a7_1.f
! %VERIFY: fArg005a7_1.out:fArg005a7_1.vf
! %STDIN:
! %STDOUT: fArg005a7_1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited
!                               poly-allocatable dummy-arg to be only associated
!                               with the unlimited poly-allocatable actual-arg;
!                               use of the derived type to test the vft for the
!                               finalizer)
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

        final :: finalizeBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    contains

    subroutine createData (x, x1)
        class (*), allocatable, intent(out) :: x
        class (*), intent(in) :: x1

        allocate (x, source=x1)
    end subroutine

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program fArg005a7_1
use m
    type (child(4,1,20)) :: c1

    class (*), allocatable :: x

    allocate (base(4) :: x)

    print *, 'calling createData'

    call createData (x, c1)

    print *, 'calling deallocate'

    deallocate (x)
end
