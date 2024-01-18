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
! %GROUP: ftpbnd513.f
! %VERIFY: ftpbnd513.out:ftpbnd513.vf
! %STDIN:
! %STDOUT: ftpbnd513.out
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
!*  DATE                       : 03/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : specific type bound (type-bound functon
!*                               reference via array element)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                :
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
        integer*4 :: id

        contains

        procedure, non_overridable :: getID => baseID
    end type

    type, extends (base) :: child
        character*20 :: name

        contains

        procedure, non_overridable :: getName => ChildName
    end type

    contains

    integer*4 function baseID (b)
        class (base), intent(in) :: b

        baseID = b%id
    end function

    character(20) function ChildName (c)
        class (child), intent(in) :: c

        ChildName = c%name
    end function

    character*2 function int2Char (i)
        integer, intent(in) :: i

        write (int2Char, '(i2)') i
    end function
end module

program ftpbnd513
use m
    type (child) :: c1(10)

    c1%base = (/(base(i), i=10,19)/)

    c1%name = (/('c1_'//int2Char(c1(i)%id), i=1,10)/)

    do i = 1, 10
        if (c1(i)%getID() /= 9+i) error stop 1_4
        print *, c1(i)%getName()
    end do

end
