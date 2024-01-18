! GB DTP extension using:
! ftcx_dtp -qck -ql -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd504d.f
! opt variations: -qnock -qnol -qreuse=none

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
!*  DESCRIPTION                : specific type bound (binding name reduction is
!                               not allowed: i.e. a public inherited binding can
!                               not be overridden to be private)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

end module

module m1
use m
    type, extends (base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name
    end type
end module

module m2
use m1
    type, extends(child) :: gen3    ! (20,4,1)
        logical(k1) :: flag

        contains
        private
        procedure :: print => printG3  !<-- illegal
    end type

    contains

    subroutine printG3 (b)
        class (gen3(*,4,1)), intent(in) :: b
    end subroutine
end module

program ftpbnd504d
end
