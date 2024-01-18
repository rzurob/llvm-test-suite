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
! %GROUP: fpAssgn026a.f
! %VERIFY: fpAssgn026a.out:fpAssgn026a.vf
! %STDIN:
! %STDOUT: fpAssgn026a.out
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
!*  DATE                       : 12/21/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : pointer assignment (poly function return
!                               results as the target; use the allocatable
!                               component for the derived type)
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
        integer(8), allocatable :: data(:)

        contains

        procedure :: printMore => nothing2print
    end type

    type, extends (base) :: child
        character(20), allocatable :: name

        contains

        procedure :: printMore => printChildName
    end type

    interface
        class (base) function genData (d1, name)
        import base
            integer(8), allocatable, intent(in) :: d1(:)
            character(*), optional, intent(in) :: name
            pointer genData
        end function
    end interface

    contains

    subroutine nothing2Print (b)
        class (base), intent(in) :: b
    end subroutine

    subroutine printChildName (b)
        class (child), intent(in) :: b

        if (allocated (b%data)) then
            if (allocated (b%name)) then
                print *, 'data = ', b%data, '; name= ', b%name
            else
                print *, 'data = ', b%data, 'name not allocated'
            end if
        end if
    end subroutine
end module

program fpAssgn026a
use m
    class (base), pointer :: b1
    integer(8), allocatable :: d1(:)

    b1 => genData (d1)

    associate (x => b1)
        if (allocated (x%data)) error stop 1_4
        call x%printMore
    end associate

    allocate (d1(0:1), source=(/10_8, 11_8/))

    b1 => genData (d1, 'xlftest 101')

    associate (x => b1)
        if ((lbound(x%data,1) /= 0) .or. (ubound(x%data,1) /= 1)) error stop 2_4
        call x%printMore
    end associate
end

class (base) function genData (d1, name)
use m, only: base, child
    integer(8), allocatable, intent(in) :: d1(:)
    character(*), optional, intent(in) :: name
    pointer genData

    if (present(name)) then
        allocate (genData, source=child(d1, name))
    else
        allocate (genData, source=base(d1))
    end if
end function
