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
! %GROUP: fselTyp503a.f
! %VERIFY: fselTyp503a.out:fselTyp503a.vf
! %STDIN:
! %STDOUT: fselTyp503a.out
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
!*  DATE                       : 12/14/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : select type (defined operator used as selector)
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
        integer(4) id
    end type

    type, extends(base) :: child
        character (15) :: name
    end type

    interface operator(.gen.)
        class (base) function genData (b1)
        import base
            allocatable genData
            class (base), intent(in) :: b1
        end function
    end interface
end module


program fselType503a
use m
    select type (x => .gen. (base(10)))
        type is (base)
            print *, x
        type is (child)
            print *, x
    end select

    select type (x => .gen. (child (1, 'xlftest')))
        type is (base)
            print *, x
        type is (child)
            print *, x
    end select
end

class (base) function genData (b1)
use m, only : base
    allocatable genData
    class (base), intent(in) :: b1

    allocate (genData, source=b1)
end function
