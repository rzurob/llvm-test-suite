!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Operator: use associate name and see if
!*                                         operator of generic tb can be used
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
      character(6) :: c = 'xxxxxx'
      contains
         procedure :: concat
         generic :: operator ( // ) => concat
   end type

   contains

      type(base) function concat (a,b)
        class(base), intent(in) :: a, b

         concat%c(1:6) = a%c(1:3) // b%c(4:6)

      end function

end module

program genericOperatorAssociateName001
   use m

   type(base) :: b1, b3
   class(base), allocatable :: b2

   b1 = base('abcdef')
   allocate ( b2 , source = base('ABCDEF') )

   associate ( a => b1, b => b2 )

      b3 = a // b
      print *, b3

      associate ( c => b3 , d => b1 )

         b1 = c // d
         print *, b1

         associate ( e => b1, f => b2 )
            c = f // e
         end associate

         print *, c

      end associate

   end associate

end program
