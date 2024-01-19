!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : C403: character with deferred length as structure component without
!*                                     pointer or allocatable attributes
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
   end type

   type, extends(base) :: child
      character(:) :: c1
   end type

end module

program deferLenC403_001d

   character(:) c2

   contains

      subroutine bar(c3)
         character(:) :: c3
      end subroutine

end program

character(:) function c4()
end function
