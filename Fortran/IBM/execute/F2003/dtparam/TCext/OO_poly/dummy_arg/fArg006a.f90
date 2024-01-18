! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg006a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg006a.f
! %VERIFY: fArg006a.out:fArg006a.vf
! %STDIN:
! %STDOUT: fArg006a.out
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
!*  DESCRIPTION                : argument association (allocatable dummy-arg
!*                               allowed to be associated with unallocated
!*                               actual-arg)
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

    subroutine printNdestroy (b)
        class (base(4)), allocatable, intent(inout) :: b

        if (allocated(b)) then
            call b%print

            deallocate (b)
        else
            print *, 'there is nothing to do'
        end if
    end subroutine
end module

program fArg006a
use m
    class (base(4)), allocatable :: b1

    call printNdestroy (b1)

    allocate (b1, source = base(4)(10))

    call printNdestroy (b1)


    call printNdestroy (b1)

    allocate (b1, source = child(4,1,20) (20, 'b1'))

    call printNdestroy (b1)
end
