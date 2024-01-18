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
! %GROUP: fArg030a1.f
! %VERIFY: fArg030a1.out:fArg030a1.vf
! %STDIN:
! %STDOUT: fArg030a1.out
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
!*  DATE                       : 11/11/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (poly allocatable
!                               component in argument association; use poly
!                               actual-arg with non-poly dummy-arg)
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
        integer*4 :: id = 1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name = 'no-name'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

module m1
use m
    type container
        class (base), allocatable :: data(:)
    end type

    contains

    subroutine printData (co)
        type (container), intent(in) :: co(5)

        do i = 1, 5
            if (allocated(co(i)%data)) then
                print *, 'data element: ', i

                do j = lbound(co(i)%data,1), ubound(co(i)%data,1)
                    call co(i)%data(j)%print
                end do
            end if
        end do
    end subroutine
end module

program fArg030a
use m1
    class(container), allocatable :: co1(:)

    allocate (co1(10))

    do i = 1, 5
        allocate (co1(i)%data(i:2*i), source=(/(child(j, 'test_part_1'), &
                        j=i,2*i)/))
    end do


    call printData (co1)
end
