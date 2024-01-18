! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : B is extended from A, each type specifies a
!*				 kind parameter
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


        type A (k1)
            integer, kind :: k1 = 4
            integer(k1) :: a1
        end type

        type, extends(A) :: B (k2)
            integer, kind :: k2=k1
            integer(k2), allocatable :: a2
        end type

        type(B(4)), allocatable :: b1
        class(A), allocatable :: b2

        allocate(b1, source = B(4,4) )
        call move_alloc(b1, b2)

        select type (b2)
	     type is (B(4))
		  if ( b2%a1 /=4 ) error stop 21
		  if ( b2%a2 /=4 ) error stop 23
	end select
end
