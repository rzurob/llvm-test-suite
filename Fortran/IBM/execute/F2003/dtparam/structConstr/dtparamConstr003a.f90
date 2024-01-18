!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 02/24/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Procedure target in the structure
!                               constructor.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, kind :: n

        character(20) :: name

        procedure(character(n)), pointer, nopass :: p
    end type
end module

program dtparamConstr003a
use m
    integer, parameter :: i_const(2) = (/10, 20/)

    type (base(i_const(1))) b1
    type (base(i_const(2))) b2

    character(10), external :: ch1
    character(20), external :: ch2

    b1 = base(10)('b1 in the main program', ch1)
    b2 = base(20)('b2 in the main program', ch2)

    if (b1%p (b1%name) /= 'b1 in the ') error stop 1_4
    if (b2%p(b2%name, 5) /= 'g7%ns%ymj%rfns%uwtlw') error stop 2_4
end

character(10) function ch1 (c)
    character(*), intent(in) :: c

    ch1 = c
end function

character(20) function ch2 (c, ishift)
    character(*), intent(in) :: c
    integer, intent(in) :: ishift

    character(len(c)) :: temp

    do i = 1, len(c)
        temp(i:i) = char(ichar(c(i:i)) + ishift)
    end do

    ch2 = temp
end function
