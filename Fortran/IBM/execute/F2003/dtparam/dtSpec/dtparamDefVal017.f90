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
!*  DATE                       : 02/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Type-spec in array constructor.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k = 8
        integer, len  :: n = 42

        real(k) :: data(n)
    end type

    type, extends(base) :: child (l)
        integer, len :: l = 20

        character(l) :: name
    end type

    type A (k, n)
        integer, kind :: k = 4
        integer, len :: n = 32

        class(base(k,n)), allocatable :: data(:)
    end type
end module

program dtparamDefVal017
use m
    type(A), allocatable :: a1(:)
    class(A(8, :)), pointer :: a2(:)

    class(A), pointer :: aa3, aa4(:,:)
    type(A(8, :)), allocatable :: aa5, aa6

    logical(4), external :: precision_r4, precision_r8

    !! set up values for aa3, aa4
    allocate (aa3, aa4(1,0:1))
    allocate (aa3%data(0:1), source=(/child(4,32)((/(i*1.0e0, i=1,32)/), &
            'aa3%data(0)'), child(4,32)(1.2e1, 'aa3%data(1)')/))


    allocate(aa4(1,0)%data(0:0), source=base(4,32)((/(sin(i+1.0), i=1, 32)/)))
    allocate(aa4(1,1)%data(3), source=child(4,32)((/(i*1.2e0, i=1, 32)/), &
            'aa4(1,1) all elements'))


    !! set up values for aa5, aa6
    allocate (A(8, 42) :: aa5, aa6)
    allocate (aa5%data(10), source=(/(base((/(exp(j*1.0d0), j=0,aa5%n-1)/)), i=1,10)/))

    allocate (aa6%data(-1:8), source=(/(child((/(sqrt(j*1.0d0), j=0, aa6%n-1)/),&
                'aa6%data('//char(ichar('0')+i-1)//')'), i=1, 10)/))


    allocate (a1(3))

    a1 = (/A :: aa3, aa4/)

    allocate(a2(2), source=(/A(8, 42) :: aa5, aa6/))

    !! verify results
    do i = 1, a1%n
        if (.not. precision_r4(a1(1)%data(0)%data(i), i*1.0e0)) error stop 1_4

        if (.not. precision_r4(a1(1)%data(1)%data(i), 1.2e1)) error stop 2_4

        if (.not. precision_r4(a1(2)%data(0)%data(i), sin(i+1.0))) &
            error stop 3_4

        if (.not. precision_r4(a1(3)%data(1)%data(i), i*1.2e0)) error stop 4_4
        if (.not. precision_r4(a1(3)%data(2)%data(i), i*1.2e0)) error stop 5_4
        if (.not. precision_r4(a1(3)%data(3)%data(i), i*1.2e0)) error stop 6_4
    end do

    select type (x => a1(1)%data)
        type is (child(4,*,*))
            if ((x(0)%name /= 'aa3%data(0)') .or. (x(1)%name /= 'aa3%data(1)'))&
                    error stop 7_4

        class default
            stop 30
    end select

    select type (x => a1(3)%data)
        type is (child(4,*,*))
            if (any(x%name /= 'aa4(1,1) all element')) error stop 8_4

        class default
            stop 31
    end select

    do i = 1, 10
        do j = 0, a2%n-1
            if (.not. precision_r8(a2(1)%data(i)%data(j+1), exp(j*1.0d0))) &
                    error stop 9_4

            if (.not. precision_r8(a2(2)%data(i-2)%data(j+1), sqrt(j*1.0d0))) &
                    error stop 10_4
        end do
    end do

    select type (x => a2(2)%data)
        type is (child(n=*,l=*))
            do i = -1, 8
                if (x(i)%name /= 'aa6%data('//char(ichar('0')+i+1)//')')&
                    error stop 11_4
            end do

        class default
            stop 32
    end select
end
