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
!*  DESCRIPTION                : scalar character with deferred length scalar/arrays
!*                               allocate with associate name
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

program deferLenAllocate029

   character(:), allocatable :: c1, c2, c3(:)

   associate ( g => "abcdefghi" )
      allocate ( c1, source = g )

      print *, len(c1), c1

      associate ( h => c1 )

         allocate ( c2, source = g // h )
         print *, len(c2), c2

         allocate ( c3(2), source = (/ character(len=9) :: c1, c2(2:10) /) )
         print *, len(c3), c3

      end associate

   end associate

end program
