! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr041d.f
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
!*  DATE                       : 04/23/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : structure constructor (DATA statement for
!                               derived type with nonpointer component default
!                               initialized)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    implicit type(child(4,1,20)) (c)
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = 0
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name  = ''
    end type


!    type (child) :: c1_m, c2_m
    save c1_m, c2_m

    data c1_m / child(4,1,20)(1, 'c1_m') /      !<-- illegal

    data c2_m%base / base(4)(2) /          !<-- illegal
!    data c2_m%name / 'c2_m' /
!    data c2_m%id / 1 /
end module

program fconstr041d
end
