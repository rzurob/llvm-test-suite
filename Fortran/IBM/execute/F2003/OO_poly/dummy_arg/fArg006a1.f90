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
! %GROUP: fArg006a1.f
! %VERIFY: fArg006a1.out:fArg006a1.vf
! %STDIN:
! %STDOUT: fArg006a1.out
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
!*  DATE                       : 05/06/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (unlimited
!*                               poly-allocatable actual-arg associated with
!*                               dummy-arg; unallocated actual-arg allowed)
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

    subroutine deallocateX (x)
        class (*), allocatable, intent(inout) :: x

        if (allocated (x)) then
            deallocate (x)
        else
            print *, 'not allocated'
        end if
    end subroutine
end module

program fArg006a1
use m
    class (*), allocatable :: x1

    print *, 'first call'

    call deallocateX (x1)

    allocate (integer*4 :: x1)

    print *, 'second call'

    call deallocateX (x1)

    print *, 'last call'

    call deallocateX (x1)
end
