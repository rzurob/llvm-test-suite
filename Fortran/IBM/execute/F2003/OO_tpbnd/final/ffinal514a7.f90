!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal514a7.f
! %VERIFY: ffinal514a7.out:ffinal514a7.vf
! %STDIN:
! %STDOUT: ffinal514a7.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization of temps created by
!*                               function call in a pointer assignment
!*                               statement)
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

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    function produceBasePointer (b)
        type (base), pointer :: produceBasePointer
        type (base), intent(in) :: b

        type (base), save, target :: temp

        temp%id = b%id

        produceBasePointer => temp

        print *, 'returning from produceBasePointer'
    end function

    type (base) function produceBaseObj (i)
        integer*4, intent(in) :: i

        produceBaseObj%id = i
        print *, 'returning from produceBaseObj'
    end function
end module

program ffinal514a7
use m
    type (base), pointer :: b_ptr

    b_ptr => produceBasePointer (produceBaseObj(10))

    if (b_ptr%id /= 10) error stop 1_4
end
