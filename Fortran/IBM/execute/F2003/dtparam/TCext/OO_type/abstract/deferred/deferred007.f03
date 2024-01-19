! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/deferred/deferred007.f
!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Deferred Binding
!*                                  - Deferred Binding with polymorphic array function result
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

module n

   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure(inf), deferred, pass :: make
   end type

   interface
      function inf(dtv)
         import base
         class(base(4)), intent(inout) :: dtv
         class(base(4)), allocatable :: inf(:)
      end function
   end interface

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      integer(k2)   :: j
      contains
         procedure, pass :: make => makechild
   end type

   type, extends(child) :: gen3(k3)    ! (4,4,4)
      integer, kind :: k3
      integer(k3)   :: k
      contains
         procedure, pass :: make => makegen3
   end type

   contains

      class(base(4)) function makechild(dtv)
         class(child(4,4)), intent(inout) :: dtv
         allocatable :: makechild(:)

         allocate ( makechild(3), source = (/ dtv, dtv, dtv /) )

      end function

      function makegen3(dtv) result(makechild)
         class(gen3(4,4,4)), intent(inout) :: dtv
         class(base(4)), allocatable :: makechild(:)

         allocate ( makechild(5), source = (/ ( dtv, i=11,15 ) /) )

      end function


end module

program deferred007
   use n

   class(base(4)), allocatable, target :: b1(:)
   class(base(4)), pointer     :: b2(:)

   allocate ( b1(3), source = (/ child(4,4)(1,11) , child(4,4)(2,22) , child(4,4)(3,33) /) )
   b2 => b1

   allocate ( b2(3), source = b1(2)%make() )

   select type ( b1 )
      type is ( child(4,4) )
         if ( ( b1(1)%i /= 1 ) .or. ( b1(2)%i /= 2 ) .or. ( b1(3)%i /= 3 ) .or. &
              ( b1(1)%j /= 11 ) .or. ( b1(2)%j /= 22 ) .or. ( b1(3)%j /= 33 ) )  error stop 1_4
      class default
         error stop 2_4
   end select

   select type ( b2 )
      type is ( child(4,4) )
         if ( ( b2(1)%i /= 2 ) .or. ( b2(2)%i /= 2 ) .or. ( b2(3)%i /= 2 ) .or. &
              ( b2(1)%j /= 22 ) .or. ( b2(2)%j /= 22 ) .or. ( b2(3)%j /= 22 ) )  error stop 3_4
      class default
         error stop 4_4
   end select

   deallocate ( b1, b2 )

   allocate ( b1(3), source = (/ gen3(4,4,4)(1,11,111), gen3(4,4,4)(1,11,111), gen3(4,4,4)(6,66,666) /) )

   do i=1,3
      if ( associated( b2 ) ) then
         nullify( b2 )
      end if
      allocate ( b2(5), source = (/ b1(i)%make() /) )
   end do

   select type ( b2 )
      type is ( gen3(4,4,4) )
         if ( ( b2(1)%i /= 6 ) .or. ( b2(2)%i /= 6 ) .or. ( b2(3)%i /= 6 ) .or. ( b2(4)%i /= 6 ) .or. ( b2(5)%i /= 6 ) .or. &
              ( b2(1)%j /= 66 ) .or. ( b2(2)%j /= 66 ) .or. ( b2(3)%j /= 66 ) .or. ( b2(4)%j /= 66 ) .or. ( b2(5)%j /= 66 ).or. &
              ( b2(1)%k /= 666 ) .or. ( b2(2)%k /= 666 ) .or. ( b2(3)%k /= 666 ) .or. ( b2(4)%k /= 666 ) .or. ( b2(5)%k /= 666 ) ) error stop 5_4
      class default
         error stop 6_4
   end select

end program
