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
!*  DATE                       : 03/09/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: diagnostic test: The same rules apply for
!                               derived type component even the defined
!                               assignment is defined for the two types: use
!                               interfac block.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    interface assignment(=)
        procedure assgnBase4toBase8
    end interface

    contains

    subroutine assgnBase4toBase8 (b8, b4)
        type(base(8,*)), intent(out) :: b8
        type(base(4,*)), intent(in) :: b4

        print *, 'defined assignment'
    end subroutine
end module

module n
use m
    type container (k, n)
        integer, kind :: k
        integer, len :: n

        type(base(k,n)) :: data
    end type
end module

program dtparamConstr030d2
use m
use n
    type (base(8,:)), allocatable :: b1
    type (base(4,:)), allocatable :: b2

    type(container(8,:)), allocatable :: co1

    allocate (base(8,20) :: b1)
    allocate (base(4,20) :: b2)
    allocate (container(8,20) :: co1)

    co1 = container(8,20)(b1)   !<-- correct use
    co1 = container(8,20)(b2)   !<-- incorrect use of structure constructor
end
