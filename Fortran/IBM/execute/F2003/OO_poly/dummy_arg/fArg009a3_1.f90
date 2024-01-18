!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 05/18/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (value attribute with
!                               dummy-arg; has the initial value as that of the
!                               actual-arg)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    subroutine abc (a)
        class (*) :: a

        select type (a)
            type is (integer(4))
                call printIntVal (a)

            class default
                error stop 1_4
        end select
    end subroutine

    subroutine printIntVal (a)
        integer, value :: a

        print *, a
        a = 10

        print *, a
    end subroutine
end module

program fArg009a3_1
use m
    integer(4) :: i1 (3:7)

    i1 = (/3,4,5,6,7/)

    call abc (i1(4))

    if (i1(4) /= 4) error stop 2_4
end
