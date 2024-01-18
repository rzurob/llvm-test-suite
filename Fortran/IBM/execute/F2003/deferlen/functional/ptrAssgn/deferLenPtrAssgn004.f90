!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : array character with deferred length with
!*                               points to null and test its association status
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
   character(:), pointer :: c1(:,:,:,:,:)
end module

program deferLenPtrAssgn004
   use n

   character(kind=1,len=:), pointer :: c2(:,:,:,:,:)
   character(150) :: abc

   c1 => null()
   c2 => null()

   if ( associated ( c1, c2 ) ) error stop 1_4
   if ( associated ( c1 ) ) error stop 2_4
   if ( associated ( c2, null(c2) ) ) error stop 3_4

end program
