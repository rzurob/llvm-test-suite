! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specific/ftpbnd508d.f
! opt variations: -qnol

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
!*  DATE                       : 02/24/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : specific type bound (binding name appears in
!                               the data statement)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id

        contains

        procedure :: value => baseID
    end type

    type (base(20,4)) b1_m

    data b1_m%id, b1_m%value  /1, baseID(base(20,4)(10))/     !<-- illegal syntax

    contains

    integer(4) function baseID(b)
        class (base(*,4)), intent(in) :: b

        baseID = b%id
    end function
end module


program ftpbnd508d
end
