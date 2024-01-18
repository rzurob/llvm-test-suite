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
!*  DESCRIPTION                : Operator: 12.4.5 Resolving type-bound procedure references
!*                                         iv ) contains both assumed-shape array and elemental references with interface
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


module m

   type base
      integer i
      contains
         procedure :: add
         generic :: operator (+) => add
   end type

   contains

   type(base) function add (a,b)
      class(base), intent(in) :: a, b(:)

      add%i = a%i

      do j = 1, size(b)
         add%i = add%i + b(j)%i
      end do
      print *, 'add'

   end function

end module

module n
   use m, only: base
   
   interface operator(+)
      module procedure elementaladd
   end interface
   contains
   
   type(base) elemental function elementaladd (a,b)
      class(base), intent(in) :: a, b

      elementaladd%i = a%i + b%i

   end function
end

program genericOperatorResolve004a
   use n

   type(base) :: b1, b2(4)
   type(base), allocatable :: b3(:)
   
   allocate ( b3(4) )

   b1 = base(5) + base(5)
   print *, b1%i

   b2 = b1 + (/ base(2) /)
   print *, b2%i

   b3 = b2(1) + (/ base(2), base(2), base(2) , base(2), base(2) /)
   print *, b3%i

   b2 = b2 + b3
   print *, b2%i

   associate ( g =>  reshape( source = b2, shape = (/2,2/) ) + reshape( source = b3, shape = (/2,2/) )  )
      print *, g%i
   end associate

end program
