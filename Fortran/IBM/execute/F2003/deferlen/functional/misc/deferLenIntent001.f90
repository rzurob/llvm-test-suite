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
!*  DESCRIPTION                : specify character with deferred length with intent(out)
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


program deferLenIntent001

   character(:), allocatable :: c1
   
   call foo ( c1 )
   
   print *, c1, len(c1), allocated(c1)
   
   deallocate ( c1 )
   allocate ( c1 , source = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' )
   print *, c1, len(c1)
   
   call foo ( c1 )
   
   print *, c1, len(c1)

   contains

      subroutine foo ( a )
         character(:), allocatable, intent(out) :: a
         
         print *, 'foo', allocated(a)
         
         allocate ( a, source = 'ibm')

      end subroutine

end program
