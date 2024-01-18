!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : argument association with unlimited polymorphic arrays
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

   contains

      subroutine foo ( u )
         class(*), intent(inout) :: u(:,:)

         select type ( u )
            type is ( character(*) )
               print *, u, len(u)
               u = reshape( source = (/ 'ibm', 'xlf', 'xlC', 'g8t' /), shape = (/2,2/) )
         end select

      end subroutine

end module

program deferLenArgAsso015
   use n

   character(:), allocatable, target :: c1(:,:)
   character(:), pointer :: c2(:,:)

   allocate ( c1(2,2), source = reshape( source = (/ 'abc', 'def', 'ghi', 'jkl' /), shape = (/2,2/) ) )
   c2 => c1

   call foo ( c1 )
   call foo ( c2 )

end program
