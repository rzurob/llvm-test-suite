! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/override/override004d.f
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
!*                               iv) They shall have the same number of dummy arguments
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

   type, extends(base1) :: child2(k3)    ! (4,4)
      integer, kind :: k3
      real(k3)      :: zid
      contains
         procedure, pass :: setid => setid2
   end type

   interface
      subroutine inf(dtv)
         import base1
         class(base1(4)), intent(inout) :: dtv
      end subroutine
   end interface

   contains

      subroutine setid(dtv,j)
         class(child1(4,4)), intent(inout) :: dtv
         integer, optional :: j
      end subroutine

      subroutine setid2(dtv,j)
         class(child2(4,4)), intent(inout) :: dtv
         integer :: j
      end subroutine

end module

program override004d
end program
