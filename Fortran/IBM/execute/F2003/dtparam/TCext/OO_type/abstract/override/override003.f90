! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/override/override003.f
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 05/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Type-bound procedure overriding
!*                               iii) Either both shall be elemental or neither shall
!*                                    - try both elemental
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

   type, abstract :: base1(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
      contains
         procedure(inf), deferred, pass :: setid
   end type

   type, extends(base1) :: child1(k2)    ! (4,4)
      integer, kind :: k2
      real(k2)      :: rid
      contains
         procedure, pass :: setid
   end type

   interface
      elemental subroutine inf(dtv)
         import base1
         class(base1(4)), intent(inout) :: dtv
      end subroutine
   end interface

   interface
      elemental subroutine setid(dtv)
         import child1
         class(child1(4,4)), intent(inout) :: dtv
      end subroutine
   end interface

end module

elemental subroutine setid(dtv)
   use n
   class(child1(4,4)), intent(inout) :: dtv
   dtv%id = 2
   dtv%rid= 3.0
end subroutine

program override003
   use n

   class(base1(4)), allocatable :: b1, b2(:)
   procedure(logical) :: precision_r4
   allocate ( b1, source = child1(4,4)(99,100.0) )
   allocate ( b2(3), source = (/ child1(4,4)(99,100.0), child1(4,4)(99,100.0), child1(4,4)(99,100.0) /) )

   call b1%setid()
   call b2(1:3:2)%setid()

   select type ( b1 )
      type is ( child1(4,4) )
         if ( ( b1%id /= 2 ) .or. ( .not. precision_r4(b1%rid, 3.0 ) ) ) error stop 1_4
   end select

   select type ( b2 )
      type is ( child1(4,4) )
         if ( ( b2(1)%id /= 2 )  .or. ( .not. precision_r4(b2(1)%rid, 3.0 ) )   .or. &
              ( b2(2)%id /= 99 ) .or. ( .not. precision_r4(b2(2)%rid, 100.0 ) ) .or. &
              ( b2(3)%id /= 2 )  .or. ( .not. precision_r4(b2(3)%rid, 3.0 ) ) ) error stop 2_4
   end select

   call b2%setid()

   select type ( b2 )
      type is ( child1(4,4) )
         if ( ( b2(1)%id /= 2 ) .or. ( .not. precision_r4(b2(1)%rid, 3.0 ) ) .or. &
              ( b2(2)%id /= 2 ) .or. ( .not. precision_r4(b2(2)%rid, 3.0 ) ) .or. &
              ( b2(3)%id /= 2 ) .or. ( .not. precision_r4(b2(3)%rid, 3.0 ) ) )   error stop 3_4
   end select

end program
