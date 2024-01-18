!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/27/2005
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type parameter used in the kind type
!                               param for components: in type(derived-type-spec)
!                               and class(derived-type-spec).
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k1)
        integer, kind :: k1

        integer(k1) :: id
        character(20) :: name = 'default'
    end type

    type, extends(base) :: child (k2)
        integer, kind :: k2

        real(k2), pointer :: data => null()
    end type

    type container (k)
        integer, kind :: k

        type (base(k)) b1
        class (base(k)), allocatable :: b2(:,:)
        class (child(k, 2*k)), pointer :: c1 => null()
        procedure(type(child(k, k+k))), pointer, nopass :: p => null()
    end type
end module

program kindparamInitexpr007
use m
    type (container(2)) co1

    type (container(4)), allocatable :: co2(:)

    type (container(8)) co3(2)

    logical(4) precision_r4, precision_r8, precision_r6

    !! co1
    co1%b1%id = 100

    allocate (co1%b2(2,2))
    co1%b2%id = reshape((/(i*12, i=1, 4)/), (/2,2/))

    allocate (co1%c1)
    co1%c1%id = -1
    co1%c1%name = 'co1%c1%name'

    allocate(co1%c1%data, source=1.21e1)

    !! co2
    allocate (co2(2))

    co2(2)%b1%id = 10

    allocate(co2(2)%c1)
    allocate (co2(2)%c1%data)

    co2(2)%c1%id = 2000
    co2(2)%c1%data = dsin(1.23d0)

    co2(1) = co2(2)

    !! co3
    co3(1)%b1%id = 2_8**33
    allocate (co3(1)%c1)
    allocate (co3(1)%c1%data)

    co3(1)%c1%data = qsqrt(1.44q0)

    !! verify co1
    print *, co1%b1, co1%b2%id
    print *, co1%c1%base

    if (.not. precision_r4(co1%c1%data, 12.1e0)) error stop 1_4

    !! verfiy co2(1)
    if ((co2(1)%b1%id /= 10) .or. (co2(1)%c1%id /= 2000)) error stop 2_4
    if (.not. precision_r8(co2(1)%c1%data, dsin(1.23d0))) error stop 3_4
    if (allocated (co2(1)%b2)) error stop 4_4


    !! verify co3(1)
    if (co3(1)%b1%id /2**23 /= 1024) error stop 5_4
    if (.not. precision_r6(co3(1)%c1%data, 1.2q0)) error stop 6_4
    if (allocated (co3(1)%b2)) error stop 7_4


    if (associated (co1%p) .or. associated(co2(1)%p) .or. &
        associated(co3(1)%p)) error stop 8_4
end
