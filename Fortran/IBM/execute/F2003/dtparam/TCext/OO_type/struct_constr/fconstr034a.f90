! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr034a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr034a.f
! %VERIFY: fconstr034a.out:fconstr034a.vf
! %STDIN:
! %STDOUT: fconstr034a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (unlimited
!                               poly-allocatable scalar component; data-source
!                               is of derived type with bindings, including
!                               the final binding)
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
    type container(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: data
    end type
end module

module n
    type base(k2)    ! (8)
        integer, kind :: k2
        integer(k2)   :: id

        contains

        procedure :: print => printBase
        final :: finalizeBase
    end type

    type, extends(base) :: child(k3,n2)    ! (8,1,18)
        integer, kind             :: k3
        integer, len              :: n2
        character(kind=k3,len=n2) :: name

        contains

        procedure :: print => printChild

        final finalizeChild
    end type

    contains

    subroutine printBase (b)
        class (base(8)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(8,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine finalizeBase (b)
        type(base(8)) b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child(8,1,*)) c

        print *, 'finalizeChild'
    end subroutine
end module

program fconstr034a
use n
use m
    class(*), allocatable :: x1
    class (*), pointer :: x2(:)

    type (child(8,1,18)), target :: c1 (2:10)

    allocate (x1, source=child(8,1,18)(100, 'x1'))

    c1%id = (/(i,i=2,10)/)
    c1%name = 'c1_array_of_9'

    x2 => c1

    print *, 'begin'

    associate (y1 => container(4,20)(x1), y2 => container(4,20)(x2(5)))
        select type (z => y1%data)
            class is (base(8))
                call z%print
            class default
                error stop 1_4
        end select

        select type (z => y2%data)
            class is (base(8))
                call z%print
            class default
                error stop 2_4
        end select
    end associate

    print *, 'end'
end
