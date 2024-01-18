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
! %GROUP: fmisc015a3.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
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
!*  DATE                       : 12/13/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : miscellaneous items (defect 289613; a little
!                               differece is made)
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
        integer(4), allocatable :: value
    end type

    interface operator (+)
        function b1AddI (b1, i)
        import base
            type (base), intent(in) :: b1
            integer*4, intent(in) :: i
            type (base) b1AddI
        end function

        function iAddB1 (i, b1)
        import base
            integer*4, intent(in) :: i
            type (base), intent(in) :: b1
            type (base) iAddB1
        end function
    end interface
end module

program fmisc015a3
use m
    type (base) :: b1

    b1 = base(10)

    associate (x => (1+b1), x1 => (b1 + 100))
        if ((.not. allocated(x%value)) .or. (.not. allocated(x1%value))) &
                        error stop 1_4

        if (x%value /= 11) error stop 2_4

        if (x1%value /= 110) error stop 3_4
    end associate
end

type (base) function b1AddI (b1, i)
use m, only: base
    type (base), intent(in) :: b1
    integer*4, intent(in) :: i

    allocate (b1AddI%value)

    if (allocated(b1%value)) then
        b1AddI%value = b1%value + i
    else
        b1AddI%value = i
    end if
end function

type (base) function iAddB1 (i, b1)
use m, only: base
    integer*4, intent(in) :: i
    type (base), intent(in) :: b1

    allocate (iAddB1%value)

    if (allocated(iAddB1%value)) then
        iAddB1%value = b1%value + i
    else
        iAddB1%value = i
    end if
end function
