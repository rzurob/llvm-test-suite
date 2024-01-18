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
! %GROUP: fclass004a3.f
! %VERIFY: fclass004a3.out:fclass004a3.vf
! %STDIN:
! %STDOUT: fclass004a3.out
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
!*  DATE                       : 04/01/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : class keyword (RHS as polymorphic data in an
!*                               intrinsic assignment; use arrays)
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
    end type

    type, extends(base) :: child
        character*20 :: name
    end type

    contains

    character(2) function int2Char (i)
        integer*4, intent(in) :: i

        write (int2Char, '(i2.2)') i
    end function
end module

program fclass004a3
use m
    interface operator (==)
        elemental logical function baseEqInt (b, i)
        use m
            type (base), intent(in) :: b
            integer*4, intent(in) :: i
        end function
    end interface

    class (base), pointer :: b_ptr(:)
    type (child), target :: c1 (3:12)
    type (child), target :: c2 (3:12, 2:11)

    type (base), allocatable :: b1(:)

    allocate (b1(5))

    c1 = (/(child (i, 'c1_'//int2Char(i)), i=3,12)/)

    b_ptr => c1

    b1 = b_ptr(::2)

    print *, b1

    if (.not. all (b1 == (/(i, i=3,12,2)/))) error stop 1_4

    c2 (8,:) = c1

    b_ptr => c2(8, :)

    b1 = b_ptr(2::2)

    print *, b1

    if (.not. all (b1 == (/(i, i=4,12,2)/))) error stop 2_4
end

elemental logical function baseEqInt (b, i)
use m
    type (base), intent(in) :: b
    integer*4, intent(in) :: i

    baseEqInt = (b%id == i)
end function
