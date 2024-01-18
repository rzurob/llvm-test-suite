! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specific/ftpbnd506.f
! opt variations: -ql

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
! %GROUP: ftpbnd506.f
! %VERIFY: ftpbnd506.out:ftpbnd506.vf
! %STDIN:
! %STDOUT: ftpbnd506.out
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
!*  DATE                       : 02/20/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : type bound procedure (specific used in forall
!*                               header)
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
    type base(k1)    ! (4)
        integer, kind        :: k1
        integer(k1), pointer :: i => null()

        contains

        procedure :: isAssociated
    end type

    contains

    pure logical*4 function isAssociated (b)
        class (base(4)), intent(in) :: b
        isAssociated = associated (b%i)
    end function
end module

program ftpbnd506
use m
    type (base(4)) :: b1(100)
    integer*4, target :: i1, i2

    i1 = 10
    i2 = 1

    b1(::2) = base(4)(i1)

    forall (i =1:100, .not. b1(i)%isAssociated())
        b1(i) = base(4) (i2)
    end forall

    do i=1,100
        print *, b1(i)%i
    end do
end
