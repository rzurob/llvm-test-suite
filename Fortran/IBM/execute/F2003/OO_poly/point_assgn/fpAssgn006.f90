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
! %GROUP: fpAssgn006.f
! %VERIFY: fpAssgn006.out:fpAssgn006.vf
! %STDIN:
! %STDOUT: fpAssgn006.out
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
!*  DATE                       : 02/05/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : data pointer assignment (occurred for pointer
!*                               component during the intrinsic assignment)
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
    type dataType
        contains

        procedure, nopass :: print => printData
    end type

    type, extends(dataType) :: moduleData
        integer*4 :: id

        contains
        procedure, nopass :: print => printMData
    end type

    contains

    subroutine printData
        print *, 'dataType'
    end subroutine

    subroutine printMData
        print *, 'moduleData'
    end subroutine
end module

module m1
use m

    type base
        class (dataType), pointer :: data => null()
    end type

    type, extends(base) :: child
        character*20 :: name
    end type
end module

program fpAssgn006
use m1
    type(base) :: b1, b2
    type (child) :: c1, c2

    type(dataType), target :: d1
    type(moduleData), target :: md1
    type(moduleData), pointer :: md_ptr

    call b1%data%print
    call c1%data%print

    allocate (md_ptr)

    b1 = base (data = d1)

    b2 = b1

    if (.not. associated (b2%data)) error stop 1_4

    if (associated (b2%data, d1)) error stop 10_4

    call b2%data%print

    c1 = child (md1, 'c1')

    c2 = c1

    if (.not. associated (c2%data, md1)) error stop 2_4

    call c2%data%print

    md_ptr = moduleData (100)

    c1 = child (md_ptr, 'c1')
    c2 = c1

    if (.not. associated (c2%data, md_ptr)) error stop 3_4

    call c2%data%print
end
