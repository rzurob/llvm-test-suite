!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length
!*                               as function result
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

module m

   character(len=: ), pointer :: c1

   contains

      subroutine allocateC1(i)
         integer, optional, intent(in) :: i
         if ( associated ( c1 ) ) nullify ( c1 )

         if ( present(i) ) then
            allocate ( c1, source = getlength(i) )
         else
            allocate ( c1, source = getlength(3) )
         end if

      end subroutine

      character(:) function getlength ( i )
         pointer :: getlength
         character(:), allocatable :: tmp
         integer :: j

         j = 65

         allocate ( getlength, source = '' )

         do while ( j < i+65 )

            if ( allocated ( tmp ) ) deallocate ( tmp )
            allocate ( tmp, source = getlength // CHAR(j) )

            if ( associated ( getlength ) ) nullify ( getlength )
            allocate ( getlength, source = tmp )

            j =  j + 1
         end do

      end function

end module

program deferLenPtrAssgn016
   use m

   call allocateC1()
   print *, c1, len(c1)

   call allocateC1(10)
   print *, c1, len(c1)

   call allocateC1(0)
   print *, c1, len(c1)

   call allocateC1(26)
   print *, c1, len(c1)

end program
