! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TO/FROM are component of DT, type class(*)
!*                               Pointer is type class(*)
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


    type A
       class(*), pointer :: p
    end type

    type, extends(A) :: B
       class(*), allocatable :: b1(:)
    end type

    integer, target ::  i
    integer, allocatable, target :: j(:)
    integer, allocatable, target :: k(:)

    class(*), allocatable :: x1(:), x2(:)
    class(B), target, allocatable :: y(:)

    class(*), pointer :: p(:)

    integer, target :: t1 = 101
    logical, target ::  t2 = .true.

    allocate(j(5), source =  (/ ( i, i = 1, 5) /) )
    allocate(x1(5), source =(/ ( A( j(i) ), i = 1, 5 ) /) )

    allocate(k(3), source =  (/ ( i, i = 6, 8) /) )
    allocate(x2(3), source =(/ ( A( k(i) ), i = 1,3 ) /) )

    allocate(y(2), source = (/ B(b1=x1, p = t1), B(b1=x2, p = t2 ) /) )

    p => y(1)%b1

    call move_alloc(y(1)%b1, y(2)%b1)

    if ( allocated(y(1)%b1) ) error stop 9
    if ( .not. allocated( y(2)%b1) ) error stop 10

    if ( .not. associated(p, y(2)%b1) ) error stop 11

    select type ( x => y(1)%p )
        type is (integer)
            if ( x /= 101 ) error stop 21
        class default
            stop 23
    end select

    select type ( x => y(2)%p )
        type is (logical)
            if ( x .neqv. .true. ) error stop 31
        class default
            stop 33
    end select

    select type( x => y(2)%b1 )
        type is ( A )
            select type( z => x(1)%p )
                type is (integer)
                    if ( z /= 1 ) error stop 51
                class default
                    stop 41
            end select
            select type( z => x(2)%p )
                type is (integer)
                    if ( z /= 2 ) error stop 52
                class default
                    stop 43
            end select
            select type( z => x(3)%p )
                type is (integer)
                    if ( z /= 3 ) error stop 53
                class default
                    stop 45
            end select
            select type( z => x(4)%p )
                type is (integer)
                    if ( z /= 4 ) error stop 54
                class default
                    stop 47
            end select
            select type( z => x(5)%p )
                type is (integer)
                    if ( z /= 5 ) error stop 55
                class default
                    stop 49
            end select
        class default
	    stop 57
    end select

end
