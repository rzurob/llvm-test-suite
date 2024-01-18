! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg006a2.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg006a2.f
! %VERIFY: fArg006a2.out:fArg006a2.vf
! %STDIN:
! %STDOUT: fArg006a2.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited-poly allocatable
!                               dummy-arg)
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

        final :: finalizeBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    subroutine deallocateX (x)
        class (*), allocatable, intent(inout) :: x

        if (allocated (x)) then
            deallocate (x)
        else
            print *, 'not allocated'
        end if
    end subroutine

    subroutine resetX (x)
        class (*), allocatable, intent(out) :: x(:)
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,1,*)), intent(inout) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child(4,1,*)), intent(inout) :: c(:)

        print *, 'finalizeChildRank1 size =', size(c)
    end subroutine

    subroutine finalizeBase (b)
        type (base(4)) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program fArg006a2
use m
    class (*), allocatable :: x1, x2(:)

    print *, 'first call'

    call deallocateX (x1)

    allocate (child(4,1,20) :: x1, x2 (2:3))

    print *, 'second call'

    call deallocateX (x1)

    print *, 'last call'

    call resetX (x2)

    if (allocated (x1) .or. allocated (x2)) error stop 1_4
end
