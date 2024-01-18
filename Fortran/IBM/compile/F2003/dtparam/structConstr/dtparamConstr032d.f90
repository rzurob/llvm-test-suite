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
!*  DATE                       : 03/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Rank one array used for rank two array
!                               data component: intrinsic type data.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, dim1, dim2)
        integer, kind :: k
        integer, len :: dim1, dim2

        real(k) :: data(dim1, dim2)
    end type
end module

program dtparamConstr032d
use m
    type (base(8,:,:)), pointer :: b1

    real(4) r1(200)

    allocate (base(8,10,20) :: b1)

    !! the next 2 stmts both are illegal
    b1 = base(8,10,20)(r1)

    allocate (b1, source=base(8,40,5)(r1))
end
