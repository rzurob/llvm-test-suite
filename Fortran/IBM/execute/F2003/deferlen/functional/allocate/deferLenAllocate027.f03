!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length scalar
!*                               allocate with character of assumed length
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

program deferLenAllocate027

   class(*), allocatable :: u1

   character(:), allocatable :: c1, c2

   allocate ( u1, source = "ABCDEFabcdef" ) !< length 12

   select type ( u1 )
      type is ( character(*) )
         print *, "u1 is character: ", u1
         allocate ( c1, source = u1 )
      class default
         error stop 1_4
   end select

   print *, 'c1:',len(c1), c1

   call assign ( c2, c1 )

   print *, 'c2:', len(c2), c2

   deallocate ( u1, c1 )
   allocate ( u1, source = "" )

   select type ( u1 )
      type is ( character(*) )
         print *, "u1 is character: ", u1
         allocate ( c1, source = u1 )
      class default
         error stop 1_4
   end select

   print *, 'c1:',len(c1), c1

   call assign ( c2, c1 )

   print *, 'c2:', len(c2), c2

   contains

      subroutine assign ( c1, c2 )
         character(:), allocatable, intent(inout) :: c1
         character(*), intent(in)  :: c2

         if ( allocated (c1) ) deallocate ( c1 )

         allocate ( c1, source = c2 )

      end subroutine

end program
