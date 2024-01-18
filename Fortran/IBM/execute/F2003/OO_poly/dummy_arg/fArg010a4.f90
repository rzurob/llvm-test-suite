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
! %GROUP: fArg010a4.f
! %VERIFY: fArg010a4.out:fArg010a4.vf
! %STDIN:
! %STDOUT: fArg010a4.out
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
!*  DATE                       : 05/11/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (VALUE attribute; pointer
!*                               component may be deallocated; will affect the
!*                               actual arg)
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
        integer*4, pointer :: data (:) => null()
    end type

    contains

    subroutine abc (b)
        type (base), value :: b

        if (associated (b%data)) then
            print *, 'deallocating data'

            deallocate (b%data)
        end if

        if (associated (b%data)) error stop 10_4
    end subroutine
end module

program fArg010a4
use m
    type (base) :: b1

    allocate (b1%data(10))

    b1%data = 10

    call abc (b1)

    !! after this call the memory pointed to by b1%data are freed; but the
    !caller still has a copy of descrptor that tells him otherwise.  Reference
    !to b1%data from this point will result in garbled data at the best.

    !! the following 2 calls just test the descriptor's values
    if (.not. associated (b1%data)) error stop 1_4

    if (size (b1%data) /= 10) error stop 2_4
end
