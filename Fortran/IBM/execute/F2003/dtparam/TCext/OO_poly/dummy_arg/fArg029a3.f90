! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg029a3.f
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
! %GROUP: fArg029a3.f
! %VERIFY: fArg029a3.out:fArg029a3.vf
! %STDIN:
! %STDOUT: fArg029a3.out
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
!*  DATE                       : 08/09/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (unlimited poly-function
!                               return; use sequence type to test)
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

    class (*) function replicateAnything (x)
        class (*), intent(in) :: x

        pointer replicateAnything

        allocate (replicateAnything, source=x)
    end function

    subroutine testSeq (x)
        class (*), intent(inout), target :: x

        type seq1(k1,k2)    ! (4,2)
            integer, kind :: k1,k2
            sequence

            integer(k1)      i1
            integer(k2)   :: i2
        end type

        type (seq1(4,2)), pointer :: s1

        s1 => x

        print *, s1

        s1%i1 = 2 * s1%i2
    end subroutine
end module

program fArg029a3
use m
    type seq1(k3,k4)    ! (4,2)
        integer, kind :: k3,k4
        sequence

        integer(k3)      i1
        integer(k4)      i2
    end type

    type (seq1(4,2)), pointer :: s1

    s1 => replicateAnything (seq1(4,2) (2, 4))

    if ((s1%i1 /= 2) .or. (s1%i2 /= 4_2)) error stop 1_4

    call testSeq (s1)

    if ((s1%i1 /= 8) .or. (s1%i2 /= 4_2)) error stop 2_4

end
