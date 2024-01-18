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

   type, abstract :: base1
      integer :: id
      contains
         procedure(inf), deferred, pass :: setid
   end type

   type, extends(base1) :: child1
      real :: rid
      contains
         procedure, pass :: setid
   end type

   type, extends(base1) :: child2
      real :: zid
      contains
         procedure, pass :: setid => setid2
   end type

   interface
      subroutine inf(dtv)
         import base1
         class(base1), intent(inout) :: dtv
      end subroutine
   end interface

   contains

      subroutine setid(dtv,j)
         class(child1), intent(inout) :: dtv
         integer, optional :: j
      end subroutine

      subroutine setid2(dtv,j)
         class(child2), intent(inout) :: dtv
         integer :: j
      end subroutine

end module

program override004d
end program
