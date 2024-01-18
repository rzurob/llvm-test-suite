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
!*  DATE                       : 11/28/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Statements: An extended type includes all of the
!                               type parameters of its parent.  Additional type
!                               parameters may be declared in the definition of
!                               the extended type.
!
!                               Case: in the rare cases type extension can
!                               happen in the internal subprogram.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtparamExtends013
    type base(k1)
        integer, kind :: k1 = 4

        integer(8) ids(k1)      !<-- use kind type param in place of length
    end type

    type(base(12)) b1       !<-- dummy-arg not supported yet

    if (f1() /= 38) error stop 1_4

    contains

    pure integer(selected_int_kind(10)) function f1()

        type, extends(base) :: child(k2,l)
            integer, kind :: k2 = k1
            integer, len :: l = k2      !<-- kind type parameter is init. expr.

            real(k2) data(l)
        end type

        type (child) c1
        type (child(10, 8)) c2

        f1 = size(b1%ids)
        f1 = f1 + size(c1%base%ids) + size(c2%ids)
        f1 = f1 + size(c1%data) + size(c2%data)
    end function
end
