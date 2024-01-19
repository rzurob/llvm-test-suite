!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length with
!*                               argument association and used in function result
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
   character(:), allocatable :: c1, c2
end module

program deferLenArgAsso005
   use n

   allocate ( c2, source = foo ( c1 ) )

   print *, "outside: ", c2, "|", len(c2)

   contains

      character(:) function foo ( c )
         character(:), allocatable :: c
         allocatable :: foo

         allocate ( c, source = "abcdefghi" )
         print *, "inside foo: ", c, "|", len(c)

         allocate ( foo, source = c // c1 )

      end function

end program
