! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are unlimited polymorphic,
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type A
        integer iA
    end type

    type, extends(A) :: B
        integer iB
        class(*), allocatable  :: b1(:)
    end type

    integer i

    class(*), allocatable :: x1(:), x2(:)
    class(*), allocatable :: y(:)

    allocate(x1(5), source = (/ ( A(i), i = 1, 5 ) /) )
    allocate(x2(3), source = (/ ( A(i), i = 6,8 ) /) )

    allocate(y(2), source = (/ B(iA=11, iB=101,b1=x1), B(iA=12, iB=102, b1=x2 ) /))

    select type (y)
        type is (B)
            call move_alloc( y(1)%b1 , y(2)%b1 )
            if ( allocated(y(1)%b1 )) error stop 31
            if ( .not. allocated(y(2)%b1) ) error stop 33
            select type ( arg => y(2)%b1 )
        	type is (A)
                    print *, arg%iA
                class default
                   stop 41
            end select
        class default
           stop 51
    end select

   end
