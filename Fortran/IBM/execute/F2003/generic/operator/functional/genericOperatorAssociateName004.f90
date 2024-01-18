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
!*                                         operator of generic tb can be used for component
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

   type dt
      integer i
      contains
         procedure :: adddt
         generic :: operator ( + ) => adddt
   end type

   type base
      type(dt) :: d
      contains
         procedure :: add
         generic :: operator ( + ) => add
   end type

   contains

      type(base) function add ( a, b )
         class(base), intent(in) :: a, b

         add%d = a%d + b%d

      end function

      type(dt) function adddt ( a, b )
         class(dt), intent(in) :: a, b

         adddt%i = a%i + b%i

      end function

end module

program genericOperatorAssociateName004
   use m

   class(base), allocatable :: b1, b2
   type(base) :: b3

   allocate ( b1, source = base (dt ( 10 )) )
   allocate ( b2, source = base (dt ( 20 )) )

   associate ( g => b1 , h => b2 )
      b3 = g + h
      print *, b3
      b3%d%i = 0
      associate ( g => b1%d , h => b2%d )
         b3%d = g + h
         print *, b3
      end associate
   end associate

   select type ( d => b1 )
      class default
         b3 = d + b2
         print *, b3
         select type ( e => b2 )
            class default
               b3 = e + d + e + d
               print *, b3
         end select
   end select

end program
