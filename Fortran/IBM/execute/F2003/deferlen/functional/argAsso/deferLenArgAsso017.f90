!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : argument association with several levels of arg asso and unlimited dummy args
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
         character(*), intent(inout) :: c
      end subroutine
   end interface

   contains

      subroutine one ( c )
         class(*), intent(inout) :: c

         select type ( c )
            type is ( character(*) )
               print *, "one: ", len(c), c
               call two(c)
         end select

         contains

            subroutine two ( c )
               character(*), intent(inout) :: c

               print *, "two: ", len(c), c

               call three(c)

            end subroutine

      end subroutine

      subroutine three ( c )
         class(*), intent(inout) :: c

         select type ( c )
            type is ( character(*) )
               print *, "three: ", len(c), c
               call four(c)
         end select

      end subroutine

end module

subroutine four ( c )
   character(*), intent(inout) :: c

   interface
      subroutine five ( c )
         character(*), intent(inout) :: c
      end subroutine
   end interface

   print *, "four: ", len(c), c


   call five(c)

end subroutine

subroutine five ( c )
   character(*), intent(inout) :: c

   print *, "five: ", len(c), c

end subroutine

program deferLenArgAsso017
   use n

   implicit character(:) (c)

   allocatable :: c1

   allocate ( c1, source = "IBMXLF" )

   call one ( c1 )

   print *, c1

end program
