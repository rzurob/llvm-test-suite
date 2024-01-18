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
!*  DATE                       : 02/07/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: The deferred type parameters for the
!                               components that are of parameterized derived
!                               type.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (k, n)
        integer, len:: n
        integer, kind :: k

        real(k) :: data(n) = sqrt(-1.0)
    end type

    type base (l, k, ka, na)
        integer, kind:: k, ka
        integer, len :: l, na

        class(A(ka, na)), allocatable :: data
        integer(k) :: id
        character(l) :: name
    end type

    type(base(:,4,4,:)), pointer :: b1_m(:)
end module

program typeParamOrder004a
use m
    class(base(:,8,8,:)), allocatable :: b1

    logical(4), external :: precision_r4, precision_r8

    allocate (base(25,8,8,20):: b1)
    allocate (base(15, 4, 4, 10) :: b1_m(10))

    allocate (b1%data)
    b1%data%data = (/(sinh(i*1.0d0), i=1,20)/)

    b1%id = 2_8**44
    b1%name = repeat('xlftest',4)

    b1_m%id = (/(i, i=1,10)/)
    b1_m%name=(/character(15)::('xlftestxlftest', 'xlftest', i=1,5)/)

    do i = 1, 10
        allocate (b1_m(i)%data)

        b1_m(i)%data%data = (/(i*1.0e1+j, j=0,9)/)
    end do

    !! verify
    if (b1%id/2**29 /= 32768) error stop 1_4
    if (b1%name /= 'xlftestxlftestxlftestxlft') error stop 2_4

    do i = 1, 20
        if (.not. precision_r8(b1%data%data(i), sinh(i*1.0d0))) error stop 3_4
    end do

    if (any(b1_m%id /= (/(j, j=1, 10)/))) error stop 4_4

    do i = 1, 10, 2
        if (b1_m(i)%name /= 'xlftestxlftest') error stop 5_4
        if (b1_m(i+1)%name /= 'xlftest') error stop 6_4
    end do

    do i = 1, 10
        do j = 1, 10
            if (.not. precision_r4(b1_m(i)%data%data(j), i*1.0e1+j-1)) &
                    error stop 7_4
        end do
    end do
end
