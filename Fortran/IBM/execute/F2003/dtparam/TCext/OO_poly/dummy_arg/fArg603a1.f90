! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg603a1.f
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
! %GROUP: fArg603a1.f
! %VERIFY: fArg603a1.out:fArg603a1.vf
! %STDIN:
! %STDOUT: fArg603a1.out
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
!*  DATE                       : 12/09/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (assumed-shape unlimited
!                               poly dummy-arg associated with array sections of
!                               sequence type)
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

program fArg603a1
    class(*), pointer :: i(:)

    interface
        subroutine print3 (x)
            class (*), intent (in) :: x(:)
        end subroutine
    end interface

    type seq1(k1)    ! (4)
        integer, kind :: k1
        sequence
        integer(k1)      i1, i2
    end type

    integer i2(10)

    type (seq1(4)), target :: s1 (100)

    i2 = (/1, 11, 21, 31, 41, 51, 61, 71, 81, 91/)

    s1 = (/(seq1(4)(j, j*100), j=1,100)/)

    i => s1

    call print3(i)
    call print3(i(i2))
    call print3(i(::2))
end

subroutine print3(p)
    class(*), intent(in) :: p(:)

    type seq1(k2)    ! (4)
        integer, kind :: k2
        sequence
        integer(k2)      i1, i2
    end type

    type (seq1(4)), pointer :: s1(:)

    associate (x => transfer (p, s1, 3))
        print *, x
    end associate
end subroutine
