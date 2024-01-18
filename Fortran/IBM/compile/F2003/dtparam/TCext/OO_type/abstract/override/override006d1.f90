! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/override/override006d1.f
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
!*                               vi)dummy arguments that correspond by position shall have the same names and characteristics,
!*                                  except for the type of the passed-object dummy argument
!*                                     - non-passed-object has different characteristics (intent, shape)
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
      integer(k1)   :: id
      contains
         procedure(inf), deferred, pass(dtv) :: setid
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      real(k2)      :: rid
      contains
         procedure, pass(dtv):: setid => setid1
   end type

   type, extends(base) :: child1(k3)    ! (4,4)
      integer, kind :: k3
      real(k3)      :: zid
      contains
         procedure, pass(dtv):: setid => setid2
   end type

   type, extends(base) :: child2(k4)    ! (4,4)
      integer, kind :: k4
      real(k4)      :: yid
      contains
         procedure, pass(dtv):: setid => setid3
   end type

   type, extends(base) :: child3(k5)    ! (4,4)
      integer, kind :: k5
      real(k5)      :: yid
      contains
         procedure, pass(dtv):: setid => setid4
   end type

   interface
      subroutine inf(dtv,j)
         import base
         class(base(4)), intent(inout) :: dtv
         integer, intent(in) :: j
      end subroutine
   end interface

   contains

      subroutine setid1(dtv,j)
         class(child(4,4)), intent(inout) :: dtv
         integer, intent(inout) :: j        !<= different intent
      end subroutine

      subroutine setid2(dtv,j)
         class(child1(4,4)), intent(inout) :: dtv
         integer, intent(in) :: j(:)        !<= different shape
      end subroutine

      subroutine setid3(dtv,j)
         class(child2(4,4)), intent(inout) :: dtv
         integer, intent(in), optional :: j  !<= is optional
      end subroutine

      subroutine setid4(dtv,j)
         class(child3(4,4)), intent(inout) :: dtv
         integer, intent(in), pointer :: j   !<= is pointer
      end subroutine

end module

program override006d1
end program
