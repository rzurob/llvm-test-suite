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
!*  DESCRIPTION                : Operator: Inheritance
!*                                         two different types extending base type,
!*                                         and generic operator only available in one of them
!*                                         Both types shall be able to use the generic operator if it matches the interface
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
         procedure, pass :: BaB
   end type

   type, extends(base) :: c1
   end type

   type, extends(base) :: c2
      contains
         generic :: operator(+) => BaB
   end type

   contains

      class(base) function BaB (a, b)
         class(base), intent(in) :: a, b
         allocatable :: BaB

         allocate ( BaB, source = a )
         BaB%i = BaB%i + b%i

         print *, 'BaB'

      end function

end module

program genericOperatorInheritance003
   use m

   class(base), pointer :: b1

   class(c1), allocatable :: c11, c12
   class(c2), allocatable :: c21, c22

   allocate ( c11, source = c1(10) )
   allocate ( c21, source = c2(20) )

   allocate ( c12, source = c1(30) )
   allocate ( c22, source = c2(40) )

   allocate ( b1, source = c11 + c21 )  !<- generic operator defined in c21
   print *, b1%i

   allocate ( b1, source = c12 + c22 )  !<- generic operator defined in c22
   print *, b1%i

   allocate ( b1, source = c11 + c21 + c22  )  !<- generic operator defined in c21, c22
   print *, b1%i

   associate ( gg => b1 + c21 )
      print *, gg%i
      associate ( ggg => c21 + b1 + c22 )
         print *, ggg%i
      end associate
   end associate
   
   allocate ( b1, source = c11%base + c21 )
   print *, b1%i

end program
