! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg031.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg031.f
! %VERIFY: fArg031.out:fArg031.vf
! %STDIN:
! %STDOUT: fArg031.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (dummy_arg used as actual
!                               arg)
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

    type, extends(base) :: child    ! (4,20)
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

    subroutine printBase (b)
        class (base(4,*)), intent(in) :: b

        print *, 'empty type'
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printGen3 (b)
        class (gen3(4,*,1)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printData (c)
        class (child(4,*)), intent(in) :: c

        call printBaseData (c)
    end subroutine

    subroutine printBaseData (b)
        class (base(4,*)), intent(in) :: b

        call b%print
    end subroutine

    subroutine printPtrData (c)
        class (child(4,:)), pointer, intent(in) :: c

        call printBaseData (c)
    end subroutine
end module

program fArg031
use m
    class (child(4,:)), pointer :: c1

    type (gen3(4,20,1)), target :: g1 = gen3(4,20,1) (-1, 'g1')

    allocate (c1, source=gen3(4,20,1) (100, 'c1'))

    call printData (gen3(4,20,1)(1, 'gen3_1'))

    call printData (child(4,20) (10))

    call printData (c1)

    call printPtrData (c1)

    deallocate (c1)

    c1 => g1

    call printPtrData (c1)
    call printData (c1)
end
