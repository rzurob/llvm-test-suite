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
