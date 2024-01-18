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
!*  DATE                       : 2009-10-27
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : defect 371156
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m
    type base (ki, kr)
        integer, kind :: ki, kr

        integer(ki), pointer :: id => null()
        real(kr), allocatable :: data(:)
    end type

    contains

    type(base(kind(1), kr=kind(1.0))) function genBaseDefault (id, data)
        integer, intent(in) :: id
        real, intent(in) :: data(:)

        allocate (genBaseDefault%id, source=id)
        allocate (genBaseDefault%data(size(data)), source=data)
    end function
end module

program kindparamFuncPrefix001
end
