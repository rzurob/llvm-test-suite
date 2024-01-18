!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with generic name
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : C459: define generic TB with same generic name with different access-spec
!*                                     within same derived type ( inheritance )
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

   type base
      integer :: id
      contains
         private
         procedure, pass :: setid
         generic :: set => setid
   end type

   type, extends(base) :: child
      character(3) :: c
      contains
         procedure, pass :: setname
         generic :: set => setname
   end type
   
   type, extends(base) :: child1
      character(3) :: c
      contains
         procedure, nopass :: setname
         generic :: set => setname
   end type

   contains

      subroutine setid ( a , i )
         class(base), intent(inout) :: a
         integer, intent(in) :: i

      end subroutine

      subroutine setname ( a , c )
         class(child), intent(inout) :: a
         character(3), intent(in) :: c

      end subroutine

end module

program genericC459Assignment002d
end program
