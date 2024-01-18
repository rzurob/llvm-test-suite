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
! %GROUP: ffinal514a13.f
! %VERIFY: ffinal514a13.out:ffinal514a13.vf
! %STDIN: ffinal514a13.in
! %STDOUT: ffinal514a13.out
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
!*  DATE                       : 04/19/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : final sub (finalization of function created
!*                               temps in READ and WRITE statement after use)
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
        integer*4 ::id = 1

        contains

        final :: finalizeBase, finalizeBaseRank1

        procedure :: makeArray => makeArrayFromBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine

    type (base) function makeArrayFromBase (b, n)
        class (base), intent(in) :: b
        integer*4, intent(in) ::  n
        dimension makeArrayFromBase(n)

        makeArrayFromBase%id = b%id
    end function
end module

program ffinal514a13
use m
    type (base) :: b1(3), b2(1)


    do i = 1, 3
        read (*,*) b1(size(b1(i)%makeArray(i)))
    end do

    do i = 2, 4
        write (*,*) b1(size(b1(i-1)%makeArray(i-1)))
    end do
end
