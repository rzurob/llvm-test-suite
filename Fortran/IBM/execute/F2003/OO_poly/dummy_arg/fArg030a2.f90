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
! %GROUP: fArg030a2.f
! %VERIFY: fArg030a2.out:fArg030a2.vf
! %STDIN:
! %STDOUT: fArg030a2.out
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
!*  DESCRIPTION                : argument association (poly-allocatable
!                               components in actual argument; use poly
!                               allocatable array section as the actual argument
!                               and non-poly explicit-shape array dummy-arg)
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

program fArg030a2
use m1
    class(container), allocatable :: co1(:)

    allocate (co1(10))

    do i = 1, 10, 2
        allocate (co1(i)%data(i:i+1), source=(/(child(j, 'test_part_1'), &
                        j=i,i+1)/))
    end do


    call printData (co1(::2))

    call printData (co1((/1,3,5,7,9/)))
end
