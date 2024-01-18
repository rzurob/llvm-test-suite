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
!*  DATE                       : 03/03/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 316901)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base! (k, n)
!        integer, kind :: k
!        integer, len :: n

        real(8) :: data(99)
        integer(8), pointer :: id(:)
    end type
end module

program dtparamConstr020d
use m
    abstract interface
        integer(8) function genPtr8 (i)
            integer(8), intent(in) :: i(:)
            pointer genPtr8(:)
        end function

        integer(4) function genPtr4 (i)
            integer(4), intent(in) :: i(:)
            pointer genPtr4(:)
        end function
    end interface

    procedure(genPtr8), pointer :: genPtrArray8

    procedure(genPtr4), pointer :: genPtrArray4

!    type (base(4,35)) :: b1

    type(base), allocatable :: c1
    
    allocate (base :: c1)
    
!    b1 = base(4,35)(1.0, genPtrArray4)

    c1 = base(8,99)((/(i*1.0d0, i=1,99)/), genPtrArray8)
end
