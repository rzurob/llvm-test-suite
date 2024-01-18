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
!*  DATE                       : 01/11/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters in
!                               declaration-type-spec: can NOT be used for
!                               entities or components without
!                               pointer/allocatable attribute. (C403)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program deferdparamDTSpec001d
    type base (n)
        integer, len :: n

        integer ids(n)
    end type

    type(base(:)) :: b1     !<-- illegal

    type base1(l)
        integer, len :: l

        character(l) :: name
        type(base(:)) b1    !<-- illegal
    end type
end
