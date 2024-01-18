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
!*  DATE                       : 03/08/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use of function reference that returns
!                               allocatable array for data component in
!                               structure constructor.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, dim1, dim2)
        integer, kind :: k
        integer, len :: dim1, dim2

        real(k) :: data(dim1, dim2)
    end type

    type (base(4,:,:)), pointer :: b1_m
end module

module m1
    contains

    function genAlloc4 (r1, shape)
        integer, intent(in) :: shape(2)
        real(4), intent(in) :: r1(product(shape))

        real(4), allocatable :: genAlloc4(:,:)

        allocate (genAlloc4(shape(1), shape(2)), source=reshape(r1,shape))
    end function

    function genAlloc8 (r1, shape)
        integer, intent(in) :: shape(2)
        real(8), intent(in) :: r1(product(shape))

        real(8), allocatable :: genAlloc8(:,:)

        allocate (genAlloc8(shape(1), shape(2)), source=reshape(r1,shape))
    end function
end module

program dtparamConstr029
use xlf_precision
use m
use m1
    interface genRealArray
        module procedure genAlloc4
        module procedure genAlloc8
    end interface

    real(4) r1(1000)
    real(8) d1(10000)
    logical(4), external :: precision_r8
    type (base(8, 150, 60)) :: b1

    r1 = sin(log((/(i*1.0, i=1,1000)/)))
    d1 = dcos(log10((/(i*1.0d0, i=1, 10000)/)))

    allocate (base(4, 45, 20) :: b1_m)

    b1_m = base(4,45,20)(data=genRealArray(r1, (/45,20/)))
    b1 = base(8,150,60)(genRealArray(d1, (/150,60/)))

    call verifyB1_M

    !! verify b1
    k = 1
    do j = 1, 60
      do i = 1, 150
        if (.not. precision_range_R8(abs(b1%data(i,j)), abs(dcos(log10(k*1.0d0))), 0.000000000001D0 )) &
          error stop 1_4
        k = k + 1
      end do
    end do
end

subroutine verifyB1_M
use xlf_precision
use m, only: b1_m

    logical(4), external :: precision_r4

    k = 1
    do j = 1, 20
      do i = 1, 45
        if (.not. precision_range_R4(abs(b1_m%data(i,j)), abs(sin(log(k*1.0))), 0.01)) &
            error stop 10_4
        k = k + 1
      end do
    end do
end subroutine
