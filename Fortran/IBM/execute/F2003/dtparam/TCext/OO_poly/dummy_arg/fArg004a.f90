! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg004a.f
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
! %GROUP: fArg004a.f
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
!*  DATE                       : 06/16/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (unlimited poly dummy-arg
!                               to be associated with unlimited poly actual-arg)
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

    subroutine test1 (x)
        class (*), intent(in), target :: x

        type seq1(k1,k2)    ! (4,8)
            integer, kind :: k1,k2
            sequence
            integer(k1)      i1
            integer(k2)      i2
        end type

        type (seq1(4,8)), pointer :: s1

        s1 => x

        s1%i1 = s1%i1 * 2
        s1%i2 = s1%i2 ** 3
    end subroutine
end module

program fArg004a
use m
    type seq1(k3,k4)    ! (4,8)
        integer, kind :: k3,k4
        sequence
        integer(k3)      i1
        integer(k4)      i2
    end type

    class (*), allocatable, target :: x1
    type (seq1(4,8)), pointer :: s1

    !! the next 2 statement tests the allocate syntax
    allocate (x1, source=1_4)

    deallocate (x1)

    allocate (x1, source=seq1(4,8)(1_4,2_8))

    call test1 (x1)

    s1 => x1

    if ((s1%i1 /= 2) .or. (s1%i2 /= 8)) error stop 1_4
end
