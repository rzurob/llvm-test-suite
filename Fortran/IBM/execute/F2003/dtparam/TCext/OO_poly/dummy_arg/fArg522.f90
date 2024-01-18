! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg522.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg522.f
! %VERIFY: fArg522.out:fArg522.vf
! %STDIN:
! %STDOUT: fArg522.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (cross testing with select
!                               type construct)
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
    type base(k1)    ! (8)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,15)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    contains

    subroutine printData1 (b)
        class (base(8)), intent(in) :: b

        select type (b)
            type is (base(8))
                print *, b%id
            type is (child(8,1,*))
                print *, b%id, b%name
            class default
                error stop 1_4
        end select
    end subroutine

    subroutine printDataArray (b)
        class (base(8)), intent(in) :: b(:)

        do i = 1, size(b)
            call printData1 (b(i))
        end do
    end subroutine
end module

program fArg522
use m
    class (*), allocatable :: x1, x2(:)

    allocate (x1, source=child(8,1,15)(100_8, 'x1 scalar'))

    select type (x1)
        class is (base(8))
            call printData1 (x1)
        class default
            error stop 10_4
    end select

    allocate (x2(0:1), source=(/child(8,1,15)(1_8,'x1 array 1'), child(8,1,15)(2_8,'x1 array 2')/))

    select type (x2)
        class is (base(8))
            call printDataArray(x2)
        class default
            error stop 11_4
    end select
end
