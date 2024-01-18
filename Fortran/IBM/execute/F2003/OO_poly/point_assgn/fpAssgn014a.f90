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
! %GROUP: fpAssgn014a.f
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
!*  DATE                       : 07/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : data pointer assignment (module pointer
!                               association status across multiple compilation
!                               units)
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

        procedure, nopass :: typeID => baseTypeID
    end type

    type, extends (base) :: child
        integer(4) id

        contains

        procedure, nopass :: typeID => childTypeID
    end type

    type, extends (child) :: gen3
        character(20) :: name

        contains

        procedure, nopass :: typeID => gen3TypeID
    end type

    contains

    integer(4) function baseTypeID ()
        baseTypeID = 1
    end function

    integer(4) function childTypeID ()
        childTypeID = 2
    end function

    integer(4) function gen3TypeID ()
        gen3TypeID = 3
    end function
end module

module m1
use m
    class (base), pointer :: b1 => null()
    class (base), pointer :: b2(:) => null()

    type (child), target :: c1(2:10)
    type (gen3), target :: g1 (5)

end module

program fpAssgn014a
use m1
    call test1
end

subroutine test1
use m1
    logical verifyB1B2

    if (.not. verifyB1B2 (1, 1)) error stop 1_4

    call assgnB1B2_1

    if (.not. verifyB1B2 (1, 2)) error stop 2_4

    call assgnB1B2_2

    if (.not. verifyB1B2 (2, 1)) error stop 3_4
end subroutine

subroutine assgnB1B2_1
use m1
    b1 => g1(2)%base

    b2 => g1(2:5)%child
end subroutine

subroutine assgnB1B2_2
use m1
    b1 => c1(3)

    b2 => null()
end subroutine

logical function verifyB1B2 (i1, i2)
use m1
    integer(4), intent(in) :: i1, i2

    verifyB1B2 = (b1%typeID() == i1) .and. (b2%typeID() == i2)
end function
