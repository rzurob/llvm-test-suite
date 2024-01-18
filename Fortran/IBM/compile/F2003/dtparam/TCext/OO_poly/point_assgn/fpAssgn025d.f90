! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn025d.f
! opt variations: -qnok -ql -qreuse=none

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
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fpAssgn025d.f
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
!*  DATE                       : 05/19/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : pointer assignment (correct line/col numbers
!*                              shall be given in the error message)
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
    type dataType(k1)    ! (4)
        integer, kind :: k1
        class (*), pointer :: data => null()
    end type

    type, extends (dataType) :: mData    ! (4)
        integer(k1) :: id
    end type
end module

program fpAssgn025d
use m
    class (mData(4)), allocatable :: md1     !<-- md1 should have TARGET attribute
    type (mData(4)), pointer :: md2, md3

    integer*2, target :: i1 = 100

    allocate (md1, md2, md3)

    md2 = mData(4) (i1, id = 10)

    md3 = md2
!a lot of other things commented out

    md3 = mData(4) (id = 1, data = md1)    !<-- here the compiler shall fail


    deallocate (md2, md3)
end

