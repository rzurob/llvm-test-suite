! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/deferred/deferred008.f
!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Deferred Binding
!*                                  - Deferred Binding child inheriting deferred binding from parent type
!*                                    and see if poly abstract type child can invoke deferred binding
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

module m

   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure(inf), deferred, pass(dtv) :: set
   end type

   interface
      subroutine inf ( int , dtv )
         import base
         integer, intent(in) :: int
         class(base(4)), intent(inout) :: dtv
      end subroutine
   end interface

end module

module m1
   use m, only: base

   type, abstract, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      integer(k2)   :: j
   end type

end module

module m2
   use m1, only: base, child
   type, extends(child) :: gen3(k3)    ! (4,4,4)
      integer, kind :: k3
      integer(k3)   :: k
      contains
         procedure, pass(dtv) :: set
   end type

   contains

      subroutine set ( int , dtv )
         integer, intent(in) :: int
         class(gen3(4,4,4)), intent(inout) :: dtv

         dtv%i = int
         dtv%j = int
         dtv%k = int

      end subroutine

end module

   use m2

   class(base(4)), allocatable, target :: b1(:)
   class(child(4,4)), pointer    :: c1

   allocate ( b1(3), source = (/ gen3(4,4,4)(1,11,111), gen3(4,4,4)(2,22,222), gen3(4,4,4)(3,33,333) /) )
   allocate ( c1, source = gen3(4,4,4)(4,44,444) )

   call b1(1)%set(10)

   select type ( b1 )
      type is ( gen3(4,4,4) )
         if ( ( b1(1)%i /= 10 ) .or. ( b1(1)%j /= 10 ) .or. ( b1(1)%k /= 10 ) ) error stop 1_4
   end select

   call c1%set(20)

   select type ( g => c1 )
      type is ( gen3(4,4,4) )
         if ( ( g%i /= 20 ) .or. ( g%j /= 20 ) .or. ( g%k /= 20 ) ) error stop 2_4
   end select

   select type ( b1 )
      class is ( child(4,4) )
         c1 => b1(3)
         call c1%set(50)
         select type ( b1 )
            type is ( gen3(4,4,4) )
               select type ( c1 )
                  type is ( gen3(4,4,4) )
                     if ( ( b1(3)%i /= 50 ) .or. ( b1(3)%j /= 50 ) .or. ( b1(3)%k /= 50 ) .or. &
                          ( c1%i /= 50 ) .or. ( c1%j /= 50 ) .or. ( c1%k /= 50 ) ) error stop 3_4
               end select
         end select
   end select

end