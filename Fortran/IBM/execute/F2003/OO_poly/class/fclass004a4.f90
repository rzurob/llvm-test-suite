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
! %GROUP: fclass004a4.f
! %VERIFY: fclass004a4.out:fclass004a4.vf
! %STDIN:
! %STDOUT: fclass004a4.out
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
!*  DATE                       : 12/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : CLASS keyword (combination of structure
!                               component, array and intrinsic assignment)
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
        integer(8), allocatable :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase(b)
        class (base), intent(in) :: b

        select type (b)
            type is (base)
                if (allocated (b%id)) then
                    print *, b%id
                else
                    print *, 'id not allocated'
                end if
            class default
                print *, 'incorrect type'
        end select
    end subroutine

    subroutine printChild(b)
        class (child), intent(in) :: b

        select type (b)
            type is (child)
                if (allocated (b%id)) then
                    print *, b%id, b%name
                else
                    print *, 'id not allocated;', b%name
                end if
            class default
                print *, 'wrong type'
        end select
    end subroutine
end module

module m1
use m
    type container
        class (base), pointer :: data(:) => null()

        contains

        procedure :: print => printData
    end type

    contains

    subroutine printData (co)
        class (container), intent(in) :: co

        print *, 'bounds:', lbound(co%data,1), ubound(co%data,1)

        do i=lbound(co%data,1), ubound(co%data,1)
            call co%data(i)%print
        end do
    end subroutine
end module

program fclass004a4
use m1
    type (container) :: co1(10)

    class (base), target, allocatable :: b1(:, :)

    integer(8) k

    allocate (b1(5,2), source=reshape((/(child(k, 'xlftest team'), k=1,10)/), &
                                    (/5,2/)))

    !! assign the first 5 elements of co1

    forall (i=1:5) co1(i) = container(b1(i,:))

    !! assign the last 5 elements of co1
    co1(10:6:-1) = co1(1:5)


    !! verify the results

    do i = 1, 10
        call co1(i)%print
    end do
end
