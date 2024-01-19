!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: intent(in) dummy argument being an namelist object
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program misc007

   implicit none
   integer :: j
   call abc(j)

contains
   subroutine abc(i)
      integer, intent(in) :: i
      namelist /mm/ i
      read (1,NML=mm)
   end subroutine
end

