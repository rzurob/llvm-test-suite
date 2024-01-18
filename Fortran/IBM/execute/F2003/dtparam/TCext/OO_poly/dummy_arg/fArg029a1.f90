! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg029a1.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg029a1.f
! %VERIFY: fArg029a1.out:fArg029a1.vf
! %STDIN:
! %STDOUT: fArg029a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (elemental function call
!                               used as the actual-arg for array)
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
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'
    end type

    contains

    elemental logical function less (b1, b2)
        class (base(4)), intent(in) :: b1, b2

        less = (b1%id < b2%id)
    end function

    subroutine printResult (l1)
        logical, intent(in) :: l1(:)

        do i = 1, size (l1)
            if (l1(i)) print *, i
        end do
    end subroutine
end module

program fArg029a1
use m
    class (base(4)), allocatable :: b1 (:)

    allocate(child(4,1,20):: b1(3))

    b1%id = (/1,2,3/)

    call printResult (less (b1, base(4)(3)))

    call printResult (less (base(4)(2), b1))

end
