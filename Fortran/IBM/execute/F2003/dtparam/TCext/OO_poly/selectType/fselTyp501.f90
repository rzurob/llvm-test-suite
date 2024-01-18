! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/selectType/fselTyp501.f
! opt variations: -qnok -ql

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
! %GROUP: fselTyp501.f
! %VERIFY: fselTyp501.out:fselTyp501.vf
! %STDIN:
! %STDOUT: fselTyp501.out
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
!*  DATE                       : 10/21/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : select type (test the binding call using the
!                               class is type-guard for arrays)
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
        integer, kind :: k1
        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2)    ! (4,8)
        integer, kind :: k2
        integer(k2)      id

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)) b
        print *, 'empty type'
    end subroutine

    subroutine printChild (b)
        class (child(4,8)) b

        print *, b%id
    end subroutine
end module

program fselTyp501
use m
    class (*), allocatable :: x1(:)

    allocate (x1(0:2), source=(/(child(4,8)(i), i=0,2)/))

    select type (x1)
        class is (base(4))
            call x1(0)%print
            call x1(1)%print
            call x1(2)%print
        class default
            print *, 'wrong'
    end select
end
