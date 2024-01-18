! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg038a.f
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
!*  DATE                       : 05/18/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (VALUE attribute on
!                               structure constructor and named constants)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fArg038a
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id
    end type

    type (base(4)), parameter :: b_const = base(4)(1)

    call printB(base(4)(10))
    call printB(b_const)

    contains

    subroutine printB (b)
        type (base(4)), value :: b

        print *, 'base type: ', b%id
    end subroutine
end

