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
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : generic-name: generic tb has allocatable/pointer dummy args, see if
!*                                             actual arg is checked to match when generic tb is called
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
      character(3) :: c
      contains
         procedure, nopass :: printallocbase1d
         procedure, nopass :: printptrbase2d

         generic :: print => printallocbase1d, printptrbase2d
   end type

   type, extends (base) :: child
      integer :: i =-999
   end type

   contains

      subroutine printallocbase1d (a)
         class(base), intent(in), allocatable :: a(:)

      end subroutine

      subroutine printptrbase2d (a)
         class(base), intent(in), pointer :: a(:,:)

      end subroutine

end module

program genericGenericNameArray006d
   use m

   class(base) :: b0, b1(:), b2(:,:)
   pointer :: b0, b1
   allocatable :: b2
   
   allocate ( b0, b1(10) )

   call b0%print(b1)
   call b0%print(b2)

end program
