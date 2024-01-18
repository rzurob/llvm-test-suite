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
! %GROUP: fmisc010b3.f
! %VERIFY: fmisc010b3.out:fmisc010b3.vf
! %STDIN:
! %STDOUT: fmisc010b3.out
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
!*  DATE                       : 08/11/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : miscellaneous items (defect 291545)
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
        character(15) :: name = 'default'

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        integer(4) :: id = -1

        contains

        procedure :: print => printChild
    end type

    type container
        class (base), allocatable :: data(:)

        contains

        procedure :: print => printContainer
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%name
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%name, b%id
    end subroutine

    subroutine printContainer (co)
        class (container), intent(in) :: co

        if (.not. allocated (co%data)) then
            print *, 'data not allocated'
        else
            do i = lbound(co%data,1), ubound(co%data,1)
                call co%data(i)%print
            end do
        end if
    end subroutine
end module

program fmisc010b3
use m
    class (base), allocatable :: b1(:,:)

    type (container) :: co1(3)

    allocate (child:: b1(2,3))

    b1%name = reshape ((/'b1_11', 'b1_21', 'b1_12', 'b1_22', 'b1_13', &
                        'b1_23'/), (/2,3/))


    co1 = (/(container(b1(:,j)), j=1,3)/)

    call co1(1)%print
    call co1(2)%print
    call co1(3)%print
end
