! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/override/override002.f
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
!*                               ii) If the overridden binding is pure then the overridding binding shall also be pure
!*                                   - overridding is pure, overridden is not pure
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
      integer(k1)   :: id
      contains
         procedure(inf), deferred, pass :: setid
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      real(k2)      :: rid
      contains
         procedure, pass :: setid
   end type

   interface
      subroutine inf(dtv)
         import base
         class(base(4)), intent(inout) :: dtv
      end subroutine
   end interface

   contains

      pure subroutine setid(dtv)
         class(child(4,4)), intent(inout) :: dtv
         dtv%id=5
      end subroutine

end module

program override002
   use m

   class(base(4)), allocatable :: b1
   procedure(logical) :: precision_r4

   allocate ( b1, source = child(4,4) ( 10, 11.0 ) )
   
   call b1%setid()
   
   select type ( b1 )
      type is ( child(4,4) )
         if ( ( b1%id /= 5 ) .or. ( .not. precision_r4(b1%rid, 11.0)) ) error stop 1_4
   end select

end program
