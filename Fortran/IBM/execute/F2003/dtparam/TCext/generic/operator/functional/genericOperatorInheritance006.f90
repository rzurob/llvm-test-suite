! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/operator/functional/genericOperatorInheritance006.f
! opt variations: -ql

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
!*                                         base type has multiple different specific type bound that includes into the generic
!*                                         specific type bound being overridden
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)      i
      contains
         procedure, pass :: BaB
   end type

   type, extends(base) :: c1    ! (4)
      contains
         procedure, pass :: BaB => ConeaB
         generic :: operator(+) => BaB
   end type

   type, extends(base) :: c2    ! (4)
      contains
         generic :: operator(+) => BaB
   end type

   contains

      class(base(4)) function BaB (a, b)
         class(base(4)), intent(in) :: a, b
         allocatable :: BaB

         allocate ( BaB, source = a )
         BaB%i = BaB%i + b%i

         print *, 'BaB'

      end function
      
      class(base(4)) function ConeaB (a, b)
         class(c1(4)), intent(in) :: a
         class(base(4)), intent(in)  :: b
         allocatable :: ConeaB

         allocate ( ConeaB, source = a )
         ConeaB%i = ConeaB%i + b%i

         print *, 'ConeaB'

      end function

end module

program genericOperatorInheritance006
   use m

   class(base(4)), pointer :: b1

   class(c1(4)), allocatable :: c11, c12
   class(c2(4)), allocatable :: c21, c22

   allocate ( c11, source = c1(4)(10) )
   allocate ( c21, source = c2(4)(20) )

   allocate ( c12, source = c1(4)(30) )
   allocate ( c22, source = c2(4)(40) )

   allocate ( b1, source = c11 + c21 )
   print *, b1%i
   allocate ( b1, source = c11 + c22 )
   print *, b1%i
   allocate ( b1, source = c21 + c12 )
   print *, b1%i
   allocate ( b1, source = c22 + c11 )
   print *, b1%i

end program
