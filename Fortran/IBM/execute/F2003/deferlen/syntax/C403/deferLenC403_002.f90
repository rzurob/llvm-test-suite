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
!*  DESCRIPTION                : C403: character with deferred length with 
!*                                     pointer or allocatable attributes, but
!*                                     in another statement
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

   character(:) :: c1
   allocatable :: c1

end module

program deferLenC403_002

   character(:) c2
   pointer c2
   
   contains
   
      subroutine bar(c3)
         character(:) :: c3
         pointer c3
      end subroutine

end program

character(:) function c4()
   allocatable c4
   
   allocate ( c4, source = 'abc' )
end function
