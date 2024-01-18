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
!*  DATE                       : 12/12/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: diagnostic test: Extending type (with type
!                               parameters) are sequence type or BIND(C) type.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtparamExtends021d2
use iso_c_binding

    type base (k, l)
        integer, len :: l
        integer, kind :: k
    end type

    type, extends(base) :: childSeq (kk, ll)
        integer, kind :: kk
        integer, len  :: ll

        sequence            !<-- illegal

        integer(k) id(l)
        real(kk) data
        character(ll) name
    end type

    type, extends(base), bind(c) :: childBind (kk, ll)      !<--illegal
        integer, kind :: kk
        integer, len  :: ll

        integer(c_int) id(ll)
    end type
end
