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
    type base
        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        integer(8) id

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base) b
        print *, 'empty type'
    end subroutine

    subroutine printChild (b)
        class (child) b

        print *, b%id
    end subroutine
end module

program fselTyp501
use m
    class (*), allocatable :: x1(:)

    allocate (x1(0:2), source=(/(child(i), i=0,2)/))

    select type (x1)
        class is (base)
            call x1(0)%print
            call x1(1)%print
            call x1(2)%print
        class default
            print *, 'wrong'
    end select
end
