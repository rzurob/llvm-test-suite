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
! %GROUP: fpAssgn011a3_1.f
! %VERIFY: fpAssgn011a3_1.out:fpAssgn011a3_1.vf
! %STDIN:
! %STDOUT: fpAssgn011a3_1.out
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
!*  DATE                       : 05/10/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : pointer assignment (poly-pointer structure
!*                               component; scalars and arrays)
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
        integer id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    private printBase, printChild

    interface operator (//)
        function stringCat (s, i)
            character(20) stringCat
            character(*), intent(in) :: s
            integer*4, intent(in) :: i
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, 'id = ', b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, 'id = ', b%id, '; name = ', b%name
    end subroutine
end module

module m1
use m
    type container
        class(base), pointer :: data => null()

        contains

        procedure :: print => printContainer
    end type

    type container1
        class(base), pointer :: data(:) => null()

        contains

        procedure :: print => printContainer1
    end type

    private printContainer, printContainer1

    contains

    subroutine printContainer (c)
        class (container), intent(in) :: c

        if (associated (c%data)) then
            print *, 'TYPE CONTAINER'
            call c%data%print
        end if
    end subroutine

    subroutine printContainer1 (c1)
        class (container1), intent(in) :: c1

        if (associated (c1%data)) then
            print *, 'TYPE CONTAINER 1'

            do i = lbound(c1%data, 1), ubound(c1%data, 1)
                call c1%data(i)%print
            end do
        end if
    end subroutine
end module

program fpAssgn011a3_1
use m1
    type (container) :: co
    type (container1) :: co1

    type (child), target :: c1, c2(100)

    c1 = child (1, 'c1')

    c2 = (/(child(2+i, stringCat ('c2_', i+2)), i=1,100)/)

    co = container (c1)
    co1 = container1 (c2)

    call co%print
    call co%data%print

    !! print array values
    call co1%print
end

character(20) function stringCat (s, i)
    character(*), intent(in) :: s
    integer*4, intent(in) :: i

    write (stringCat, '(a,i03.03)') trim(s), i
end function
