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
! %GROUP: fconstr051a.f
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
!*  DATE                       : 10/20/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : structure constructor (poly-entities used as
!                               the data-source for nonpoly rank-one array
!                               component that is of explicit-shape array)
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
        integer(4) :: id
    end type

    type, extends(base) :: child
        character(15) :: name
    end type
end module

program fconstr051a
use m
    type container
        type (base) :: b1(2:10)
    end type

    class (base), allocatable :: b1(:)
    class (base), pointer :: b2(:)

    type (child), target :: c1(2:10)

    b2 => c1

    allocate (b1(2:10), source=(/(child(i,'b1'), i=2,10)/))

    c1%id = (/(-i, i=2,10)/)
    c1%name = 'c1_array_of_9'

    associate (x1 => container (b1), x2 => container (b2))
        if (any (x1%b1%id /= (/(j, j=2,10)/))) error stop 1_4
        if (any (x1%b1%id /= -x2%b1%id)) error stop 2_4
    end associate
end
