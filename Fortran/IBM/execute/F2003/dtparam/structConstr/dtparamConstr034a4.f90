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
!*  DATE                       : 03/16/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: correct use procedure target in structure
!                               constructor.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        procedure(produceBasePtr), pointer :: gen8
    end type

    abstract interface
        function produceBasePtr (b1, d1)
        import
            class(base(8,*)), intent(in) :: b1
            real(8), intent(in) :: d1(:)

            type(base(8,:)), pointer :: produceBasePtr
        end function
    end interface
end module

program dtparamConstr034a4
use m
    interface
        function genBase (b1, d1)
        use m
            class(base(8,*)), intent(in) :: b1
            real(8), intent(in) :: d1(:)

            type(base(8,:)), pointer :: genBase
        end function
    end interface

    type (base(8, 225)) :: b1

    real(8) :: d1(1000)

    logical(4), external :: precision_r8

    d1 = sqrt(1.0d0*(/(i, i=1,1000)/))

    b1 = base(8, 225)(d1(300:524), genBase)

    b1 = b1%gen8(d1)

    !! verify
    do i = 1, 225
        if (.not. precision_r8 (b1%data(i), &
            sqrt(1.0d0*i) + sqrt(1.0d0*(i+299)))) error stop 1_4
    end do

    b1 = b1%gen8(d1(801:))

    do i = 1, 200
        if (.not. precision_r8 (b1%data(i), &
            sqrt(1.d0*i)+sqrt(1.d0*(i+299))+sqrt(1.d0*(i+800)))) error stop 2_4
    end do

    do i = 201, 225
        if (.not. precision_r8 (b1%data(i), &
            sqrt(1.0d0*i) + sqrt(1.0d0*(i+299)))) error stop 3_4
    end do
end


function genBase (b1, d1)
use m
    class(base(8,*)), intent(in) :: b1
    real(8), intent(in) :: d1(:)

    type(base(8,:)), pointer :: genBase

    allocate (base(8,b1%n) :: genBase)

    if (b1%n > size(d1)) then
        genBase%data(:size(d1)) = b1%data(:size(d1)) + d1
        genBase%data(size(d1)+1:) = b1%data(size(d1)+1:)
    else
        genBase%data = b1%data + d1(:b1%n)
    end if

    genBase%gen8 => b1%gen8
end function
