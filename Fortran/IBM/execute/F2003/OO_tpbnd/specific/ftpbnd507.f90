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
! %GROUP: ftpbnd507.f
! %VERIFY: ftpbnd507.out:ftpbnd507.vf
! %STDIN:
! %STDOUT: ftpbnd507.out
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
!*  DATE                       : 03/30/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : specific type bound (nopass bindings for empty
!*                               type)
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
        procedure, nopass :: typeID => baseID
        procedure, nopass, non_overridable :: typeTell
    end type

    private printBase, baseID, typeTell

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b
    end subroutine

    integer function baseID ()
        baseID = 1
    end function

    integer function typeTell (b)
        class (base), intent(in) :: b

        typeTell = b%typeID()
    end function
end module

module m1
use m
    type, extends(base) :: child
        integer*4 :: id

        contains

        procedure :: print => printChild
        procedure, nopass :: typeID => childID
        procedure, nopass :: typeName => childName
    end type

    class (child), allocatable :: c1_m(:)

    contains

    subroutine printChild (b)
        class (child), INTENT(in) :: b

        print *, b%id
    end subroutine

    integer function childID ()
        childID = 2
    end function

    character*20 function childName ()

        childName = 'child'
    end function
end module

program ftpbnd507
use m1
    type (child) :: c1
    type (base) :: b1

    c1%id = 100

    if (c1%typeName () /= 'child') error stop 10_4

    call c1%print

    if (c1%typeID() /= 2) error stop 1_4

    if (c1%typeTell(c1) /= 2) error stop 2_4

    allocate (c1_m(10))

    c1_m%id = (/(i, i=1,10)/)

    if (c1_m%typeID() /= 2) error stop 3_4

    call c1_m(5)%print

    if (b1%typeTell(c1_m(5)) /= 2) error stop 4_4

    deallocate (c1_m)

    if (c1_m%typeTell(b1) /= 1) error stop 5_4
end
