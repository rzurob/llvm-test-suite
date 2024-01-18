! GB DTP extension using:
! ftcx_dtp -qck -qk -ql OO_type/struct_constr/fconstr024d.f
! (moved to F2003/dtparam/future/d343068.f, per defect 343068)
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
!*  DATE                       : 02/17/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  DESCRIPTION                : structure constructor (private parent type will
!                               result in private parent component, and is not
!                               accessible via use association)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, private :: base(k1,k2)    ! (4,4)
        integer, kind :: k1,k2
        integer(k1)   :: id
        real(k2)      :: value
    end type

    type, extends(base) :: child(k3,n1)    ! (4,4,1,20)
        integer, kind             :: k3
        integer, len              :: n1
        character(kind=k3,len=n1) :: name
    end type

    type, extends(child) :: thirdGeneration(k4)    ! (4,4,1,20,1)
        integer, kind :: k4
        logical(k4)   :: isSet
    end type

    type (base(4,4)) :: b1_m = base(4,4) (1, 1.0)
    type (child(4,4,1,20)) :: c1_m = child(4,4,1,20) (base = base(4,4)(2, 2.0), name = 'c1_m')

    type (thirdGeneration(4,4,1,20,1)) :: t1_m = thirdGeneration(4,4,1,20,1) ( &
            isSet = .true., child = child(4,4,1,20) (base = base(4,4)(3, 3.0), name = 't1_m'))
end module


program fconstr024d
use m
    type (child(4,4,1,20)) :: c1, c2

    type (thirdGeneration(4,4,1,20,1)) :: t1

    c1 = child(4,4,1,20) (name = 'c1', base = base(4,4)(4, 4.0))
    c2 = child(4,4,1,20) (name = 'c3', base = b1_m)
    t1 = thirdGeneration(4,4,1,20,1) (isSet = .true., child = child(4,4,1,20) (base = base(4,4)(5, 5.0), &
                            name = 't1'))
end
