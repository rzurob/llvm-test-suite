! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn019.f
! opt variations: -qck -qnol -qdeferredlp -qreuse=none

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
! %GROUP: fpAssgn019.f
! %VERIFY: fpAssgn019.out:fpAssgn019.vf
! %STDIN:
! %STDOUT: fpAssgn019.out
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
!*  DATE                       : 07/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : data pointer assignment (data-target is the
!                               dummy-arg)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id = 0

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child    ! (20,4)
        character(n1) :: name = ''

        contains

        procedure :: print => printChild
    end type

    class (base(20,4)), pointer :: b1_m
    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fpAssgn019
use m
    interface
        subroutine print (b)
        use m
            class (base(*,4)), intent(in), target :: b
        end subroutine
    end interface

    type (child(20,4)), target :: c1
    class (child(20,4)), allocatable :: c2
    type (base(20,4)), target :: b1(10)

    c1 = child(20,4) (10, 'c1')
    b1 = (/(base(20,4) (i), i=1, 10)/)

    allocate (c2)

    c2%id = 20
    c2%name = 'c2'

    allocate (b1_m, source=child(20,4)(100, 'b1_m'))

    call print (c1)
    call print (c2)

    call print (b1_m)
    call print (b1(4))
end


subroutine print (b)
use m
    class (base(*,4)), intent(in), target :: b

    class (base(20,4)), pointer :: b1
    type (base(20,4)), pointer :: b2

    b1 => b

    b2 => b1

    call b1%print
    call b2%print
end subroutine
