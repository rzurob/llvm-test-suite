! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/point_assgn/fpAssgn018a.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=base

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
! %GROUP: fpAssgn018a.f
! %VERIFY: fpAssgn018a.out:fpAssgn018a.vf
! %STDIN:
! %STDOUT: fpAssgn018a.out
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
!*  DATE                       : 02/24/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : data pointer assignment (module procedure as
!                               the type bound; the accessibility is obtained
!                               though use association; test case has very
!                               little to do with the pointer assignment)
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
    contains

    subroutine testSum (a, b)
        integer*4, intent(in) :: a, b

        if (a+b > 10) then
            print *, 'large'
        else
            print *, 'small'
        end if
    end subroutine
end module

program fpAssgn018a
use m

    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: value = 10

        contains

        procedure, nopass :: testAdd => testSum
    end type

    type, extends (base) :: child(k2,n2)    ! (20,4,4,20)
        integer, kind :: k2
        integer, len  :: n2
        character(n2) :: name
    end type

    class (base(20,4)), pointer :: b1
    class (base(:,4)), pointer :: b2
    type (child(20,4,4,20)), target :: c1, c2

    c1%value = 1

    call c1%testAdd (c1%value, c2%value)

    call c1%testAdd (0, c2%value)

    b1 => c2

    call abc (c1, b1)

    allocate (b1)
    b1%value = 5

    b2 => b1
    call abc (b1, b2)

    deallocate (b2)

    contains

    subroutine abc (b1, b2)
        class (base(*,4)), intent(in) :: b1, b2

        call b1%testAdd (b1%value, b2%value)
    end subroutine
end
