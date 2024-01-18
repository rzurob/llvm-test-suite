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
! %GROUP: fpAssgn018.f
! %VERIFY: fpAssgn018.out:fpAssgn018.vf
! %STDIN:
! %STDOUT: fpAssgn018.out
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
!*  DATE                       : 02/27/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : data pointer assignment (type bound can be
!*                               external procedures with explicit interface;
!*                               type defined in main program with nopass
!*                               binding referring to external procedure)
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

program fpAssgn018
type base
    contains
    procedure, nopass :: printType
    procedure, nopass :: typeID => baseTypeID
end type

type, extends (base) :: child
    CONTAINS
    procedure, nopass :: printType => printChildType
    procedure, nopass :: typeID => childTypeID
end type

interface
    subroutine printType
    end subroutine

    subroutine printChildType
    end subroutine
end interface

interface
    integer*4 function baseTypeID ()
    end function

    integer*4 function childTypeID()
    end function
end interface

    class (base), pointer :: b1 => null()
    type (child), target :: c1(100)

    call b1%printType

    if (b1%typeID() /= 1) error stop 1_4

    b1 => c1(10)

    call b1%printType

    if (b1%typeID() /= 2) error stop 2_4

    call c1(100)%printType

    if (c1%typeID() /= 2) error stop 3_4

    call abc (b1)

    call abc (c1(50))

    if (typeID (c1(35)) /= 2) error stop 4_4

    if (typeID (b1) /= 2) error stop 5_4

    nullify (b1)

    if (typeID (b1) /= 1) error stop 6_4

    call abc (b1)

    contains

    subroutine abc (b)
        class (base), intent(in) :: b

        call b%printType
    end subroutine

    integer*4 function typeID (b)
        class (base), intent(in) :: b
        typeID = b%typeID()
    end function
end

subroutine printType
    print *, 'base'
end subroutine

subroutine printChildType
    print *, 'child'
end subroutine

integer*4 function baseTypeID ()
    baseTypeId = 1
end function

integer*4 function childTypeID ()
    childTypeID = 2
end function
