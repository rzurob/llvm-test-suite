!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 06/09/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
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
    type base
        integer*4 :: id
    end type

    type, extends(base) :: child
        character*20 :: name = 'default'
    end type

    contains

    elemental logical function less (b1, b2)
        class (base), intent(in) :: b1, b2

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
    class (base), allocatable :: b1 (:)

    allocate(child:: b1(3))

    b1%id = (/1,2,3/)

    call printResult (less (b1, base(3)))

    call printResult (less (base(2), b1))

end
