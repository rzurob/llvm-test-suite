! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/10/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters in
!                               declaration-type-spec: pointer/allocatable
!                               entities.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type namedArray (l, n)
        integer, len :: l, n

        real :: data(n) = 0.0
        character(l) :: name = 'default'
    end type

    class (namedArray(:, 10)), allocatable :: na1_m(:)
end module

program deferdparamDTSpec004
use m
    type (namedArray(3,:)), pointer :: na2(:,:)

    type (namedArray(3, 100)), target :: na3(10, 5, 20)

    logical(4), external :: precision_r4

    l = 15

    allocate (namedArray(l, 10):: na1_m(10))

    na2 => na3(:,:,10)

    if ((na1_m%l /= 15) .or. (na1_m%n /= 10)) error stop 1_4
    if ((na2%l /= 3) .or. (na2%n /= 100)) error stop 2_4

    na1_m(5)%name = 'na1_m element 5...'


    if ((na1_m(2)%name /= 'default') .or. &
        (na1_m(5)%name /= 'na1_m element 5')) error stop 3_4

    if ((.not. precision_r4(na1_m(1)%data(10), 0.0)) .or. &
        (.not. precision_r4(na1_m(10)%data(1), 0.0))) error stop 4_4

    if (any (na2%name /= 'def')) error stop 5_4

    if (size(na2) /= 50) error stop 6_4

    if (size(na2(1, 5)%data) /= 100) error stop 7_4
end
