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
! %GROUP: ftpbnd510a3.f
! %VERIFY: ftpbnd510a3.out:ftpbnd510a3.vf
! %STDIN:
! %STDOUT: ftpbnd510a3.out
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
!*  DATE                       : 06/01/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : specific type bound (named constants can be of
!                           derived type with pointer or allocatable component)
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
    type node
        integer*4 :: value
        class (node), pointer :: next => null()

        contains

        procedure :: print => printNode
    end type

    type dataType
        class (node), allocatable :: data(:)

        contains

        procedure :: print => printDataType
    end type

    contains

    subroutine printNode (n)
        class (Node), intent(in) :: n

        if (associated (n%next)) then
            print *, n%value, 'next node exist'
        else
            print *, n%value, 'end of list'
        end if
    end subroutine

    subroutine printDataType (d)
        class (dataType), intent(in) :: d

        if (allocated (d%data)) then
            print *, 'data allocated'
        else
            print *, 'data not allocated'
        end if
    end subroutine
end module

program ftpbnd510a3
use m
    type (node), parameter :: default_node = node (1, null())

    type (dataType), parameter :: default_data = dataType(null())

    call default_node%print
    call default_data%print
end
