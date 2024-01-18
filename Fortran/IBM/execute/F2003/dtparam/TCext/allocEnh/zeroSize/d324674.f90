! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/zeroSize/d324674.f
! opt variations: -ql

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
!*  DATE                       : 08/30/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 324674)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind     :: k1
        real(k1), pointer :: data(:) => null()

        contains

        final :: finalizeBase
    end type

    interface base
        module procedure genBaseObj
    end interface

    contains

    subroutine finalizeBase (b)
        type(base(8)), intent(inout) :: b

        write (*, '(a)', advance='no') 'in finalizeBase; '

        if (associated(b%data)) then
            write (*, '(a, (10g12.6))') 'b%data = ', b%data
            deallocate (b%data)
        else
            print *, ''
        end if
    end subroutine

    type(base(8)) function genBaseObj (r1)
        real(8), intent(in) :: r1(:)

        allocate (genBaseObj%data(size(r1)), source=r1)
    end function
end module

program zeroSizeArray009
use m
    type (base(8)), allocatable :: b1(:)

    allocate (b1(3), source = (/(genbaseObj((/(j*1.0d0, j=1,i)/)), i=1,3)/))
end
