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
! %GROUP: fArg034a1.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
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
!*  DATE                       : 06/15/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (change of dummy-arg will
!                               be seen in the procedure by actual arg for
!                               dummy-arg with TARGET attribute)
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
        integer*4 :: id
    end type

    type, extends (base) :: child
        character*20 :: name
    end type
end module

program fArg034a1
use m
    class (base), pointer :: b1 (:)

    allocate (child :: b1(2:11))

    b1%id = (/(i, i=2,11)/)

    call test1 (b1)

    if (any (b1%id /= (/(i,i=102, 111)/))) error stop 3_4

    deallocate (b1)

    contains

    subroutine test1 (b)
        class (base), TARGET :: b (:)

        if (any (b1%id /= (/(i,i=2,11)/))) error stop 1_4

        b%id = b%id + 100

        if (any (b1%id /= (/(i,i=102, 111)/))) error stop 2_4
    end subroutine
end
