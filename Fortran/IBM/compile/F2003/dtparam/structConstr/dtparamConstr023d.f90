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
!*  DATE                       : 03/06/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Expression is of different rank for the
!                               allocatable components.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k,n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        integer(k/4) :: id
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        character(l) :: name
        logical(k) :: flag
    end type

    type container (k)
        integer, kind :: k

        class(base(k,:)), allocatable :: data(:)
    end type
end module

program dtparamConstr023d
use m
    type (child(8,30,33)) c1(2,2)

    class(base(8,:)), allocatable :: c2(:,:)

    type(container(8)) co1

    allocate(child(8,30,33) :: c2(0:1,-1:0))

    co1 = container(8)(c2)
end
