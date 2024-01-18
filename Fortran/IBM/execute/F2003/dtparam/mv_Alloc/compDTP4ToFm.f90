! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of poly type which component is of
!*				    DTP derived-type
!*                               TO is of class(*)
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type A (l,k)
        integer, len :: l
        integer, kind :: k

        character(l), allocatable :: ch
        integer(k) :: id(l)
   end type

   type B(l)
        integer, len :: l
        class(A(l=l,k=1)), allocatable :: a1
   end type

end module

use m


   class(B(8)), allocatable :: b1(:)
   class(*), allocatable :: b2(:)

   allocate(b1(2), source = (/ B(8)(A(8,1)('helloworld',(/ ( int(i,1), i=1,8)  /))), &
           B(8)(A(8,1)('IBM-compiler', (/ ( int(-i,1),i=1,8 )/))) /) )

   call move_alloc(b1, b2)

   if ( .not. allocated(b2) ) stop 21
   if ( allocated(b1) ) stop 23

   select type (b2)
        type is (B(*))
            select type ( x => b2(1)%a1)
                type is ( A(*,1))
                    print *, x%ch
                    print *, x%id
            end select

            select type ( x => b2(2)%a1)
                type is ( A(*,1))
                    print *, x%ch
                    print *, x%id
            end select
   end select
end
