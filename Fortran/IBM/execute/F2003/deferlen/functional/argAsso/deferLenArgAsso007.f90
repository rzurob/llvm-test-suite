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
!*  DESCRIPTION                : derived type containing scalar character with deferred length with
!*                               argument association, pointer component
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
   type base
      character(:), pointer :: c
   end type

   type(base) :: b1
   character(:), pointer :: c1

end module

program deferLenArgAsso007
   use n

   b1 = base(null())
   allocate ( c1, source = 'worldcup' )

   b1%c => c1
   print *, len(b1%c)

   call foo ( b1 )

   print *,'outside: ', b1%c, len(b1%c)

   contains

      subroutine foo ( c )
         type(base) :: c

         print *, 'inside: ', c%c, len(c%c)

         allocate ( c%c, source = c%c // " 2006" )

      end subroutine

end program