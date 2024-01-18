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
!*  DATE                       : 01/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Length type parameters shall not be used
!                               in initialization expressions: used in places of
!                               kind type param and default initializations.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program lenparamInitexpr001d
    type A (k)
        integer, len :: k = 4

        real(k) :: data(k)      !<-- k can NOT be used for real(k)
    end type

    type B (l)
        integer, len :: l = 10

        integer :: id = l       !<-- l can NOT be used for default init.
    end type
    end
