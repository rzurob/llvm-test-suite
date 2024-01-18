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
! %GROUP: fpAssgn009a2.f
! %VERIFY: fpAssgn009a2.out:fpAssgn009a2.vf
! %STDIN:
! %STDOUT: fpAssgn009a2.out
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
!*  DATE                       : 07/07/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : data pointer assignment (sequence type pointer
!                               assigned to unlimited poly-target; arrays)
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
    type seq1
        sequence
        integer*4 :: i1
        integer*4 :: i2
        integer*2, pointer :: i3 => null()
    end type

    contains

    subroutine printSeq (x)
        class (*), intent(in), target :: x(:)

        type (seq1), pointer :: s(:)

        s => x

        print *, (s(i)%i1, s(i)%i2, s(i)%i3, i=1,size(s))
    end subroutine
end module

program fpAssgn009a2
use m, only : printSeq
    class (*), pointer :: x1(:)

    type seq1
        sequence
        integer*4 :: i1
        integer*4 :: i2
        integer*2, pointer :: i3 => null()
    end type

    type (seq1), target :: s1(10)
    integer*2, target :: i1(10)

    i1 = (/(i*100, i=1,10)/)

    s1 = (/(seq1 (i1=i, i2=i*10, i3 = i1(i)), i=1,10)/)

    call printSeq (s1)

    x1 => s1

    call printSeq (x1)

end
