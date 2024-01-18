! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg009a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg009a.f
! %VERIFY: fArg009a.out:fArg009a.vf
! %STDIN:
! %STDOUT: fArg009a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (If the dummy-arg has the
!                               VALUE attribute it becomes associated with a
!                               definable anonymous data whose initial value is
!                               that of the actual argument.)
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
        integer, kind        :: k1
        integer(k1), pointer :: id => null()

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'no-name'

        contains

        procedure :: print => printChild
    end type


    interface printData
        subroutine printData (b)
        import
            type (base(4)), value :: b
        end subroutine

        subroutine printDataChild (b)
        import
            type (child(4,1,20)), value :: b
        end subroutine
    end interface

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        if (associated (b%id)) then
            print *, b%id
        else
            print *, 'null'
        end if
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        call b%base%print
        print *, b%name
    end subroutine

    subroutine printDataPoly (b)
        class(base(4)) :: b

        select type (b)
            class is (child(4,1,*))
                call printData(b)

            class default
                call printData(b)

        end select
    end subroutine
end module

program fArg009a
use m
    class (base(4)), allocatable :: b1, b2, b3
    integer*4, target :: i1 = 10

    allocate (b1, source=child(4,1,20)(null(), 'b1'))
    allocate (b2, source = child(4,1,20) (i1, 'b2'))

    allocate (b3)

    allocate (b3%id)

    b3%id = 100

    call printDataPoly (b1)

    call printDataPoly (b2)

    call printDataPoly (b3)
end


subroutine printData (b)
use m, only: base
    type (base(4)), value :: b

    call b%print
end subroutine

subroutine printDataChild (b)
use m, only: child
    type (child(4,1,20)), value :: b

    call b%print
end subroutine
