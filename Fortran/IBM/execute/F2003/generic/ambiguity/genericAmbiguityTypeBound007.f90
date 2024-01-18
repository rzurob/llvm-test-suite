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
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : one argument with nopass (for generic-name tb)
!*                                  - with class hierarchy: one base type and two types extending it, and defined ambiguous tb in extended types
!*                                    but actually there is no ambiguity, since the types don't collide
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

module genericName

   type base
      integer :: i
   end type

   type, extends(base) :: childa
      integer :: j
      contains
         procedure, nopass :: printa
         generic :: print => printa
   end type

   type, extends(base) :: childb
      integer :: j
      contains
         procedure, nopass :: printa => printb
         generic :: print => printa
   end type

   contains

      subroutine printa(b)
         class(base), intent(in) :: b

         print *, 'printa'

      end subroutine

      subroutine printb(b)
         class(base), intent(in) :: b

         print *, 'printb'

      end subroutine

end module

program genericAmbiguityTypeBound007
   use genericName

   class(base), allocatable :: b1

   allocate ( childa :: b1 )

   select type ( b1 )
      type is ( childa )
         call b1%print(b1)
   end select
   
   deallocate ( b1 )
   
   allocate ( childb :: b1 )

   select type ( b1 )
      type is ( childb )
         call b1%print(b1)
   end select

end program
