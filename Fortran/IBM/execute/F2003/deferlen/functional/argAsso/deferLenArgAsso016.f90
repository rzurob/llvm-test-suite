!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 05/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : argument association with several levels of arg asso
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

   interface
      subroutine four ( c )
         character(:), allocatable, intent(inout) :: c
      end subroutine
   end interface

   contains

      subroutine one ( c )
         character(:), allocatable, intent(inout) :: c

         print *, "one: ", len(c), c

         c = c // '1'

         call two(c)

         contains

            subroutine two ( c )
               character(:), allocatable, intent(inout) :: c

               print *, "two: ", len(c), c

               c = c // '2'

               call three(c)

            end subroutine

      end subroutine

      subroutine three ( c )
         character(:), allocatable, intent(inout) :: c

         print *, "three: ", len(c), c

         c = c // '3'

         call four(c)

      end subroutine

end module

subroutine four ( c )
   character(:), allocatable, intent(inout) :: c

   interface
      subroutine five ( c )
         character(:), allocatable, intent(inout) :: c
      end subroutine
   end interface

   print *, "four: ", len(c), c

   c = c // '4'

   call five(c)

end subroutine

subroutine five ( c )
   character(:), allocatable, intent(inout) :: c

   print *, "five: ", len(c), c

   c = c // '5'

end subroutine

program deferLenArgAsso016
   use n

   implicit character(:) (c)

   allocatable :: c1

   allocate ( c1, source = "IBMXLF" )

   call one ( c1 )

   print *, c1

end program
