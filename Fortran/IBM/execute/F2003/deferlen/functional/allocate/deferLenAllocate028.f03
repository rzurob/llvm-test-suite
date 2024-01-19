!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length arrays
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

program deferLenAllocate028

   class(*), allocatable :: u1(:)
   character(:), pointer :: c1(:), c2(:,:)

   allocate ( u1(6), source = (/ 'abc', 'def', 'ghi', 'jkl', 'mno', 'pqr' /) )

   select type ( u1 )
      type is ( character(*) )
         allocate ( c1(size(u1) ), source = u1 )
         allocate ( c2(2,2), source = reshape ( source = u1((/4,1,3,2/)), shape = (/2,2/) ) )
   end select

   print *, 'c1', len(c1), c1
   print *, 'c2', len(c2), c2

   deallocate ( c1, c2 )

   allocate ( c1(3), source = (/ 'abc'//'def', 'ghi'//'jkl', 'mno'//'pqr' /) )

   call assign ( c2, c1 )

   print *, 'c1', len(c1), c1, size(c1)
   print *, 'c2', len(c2), c2, size(c2,1), size(c2,2)

   contains

      subroutine assign ( c1, c2 )
         character(:), pointer, intent(inout) :: c1(:,:)
         character(*), intent(in)  :: c2(:)

         if ( associated (c1) ) deallocate ( c1 )

         allocate ( c1(1,size(c2)), source = reshape ( c2, shape = (/1, size(c2) /) ) )

      end subroutine

end program
