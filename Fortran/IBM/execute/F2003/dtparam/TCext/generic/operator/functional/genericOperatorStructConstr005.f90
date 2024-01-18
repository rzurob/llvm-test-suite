! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/generic/operator/functional/genericOperatorStructConstr005.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=base

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
!*  DESCRIPTION                : Operator: Scalar to Scalar with structure constructors within structure constructor (polymorphic components)
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

   type inner(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: j = -999
      contains
         procedure :: iadd
         generic :: operator(+) => iadd
   end type

   type, extends(inner) :: innerc(n2,k2)    ! (20,4,20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: k = -999
      contains
         procedure :: iadd => icadd
   end type

   type base(n3,k3)    ! (20,4)
      integer, kind                   :: k3
      integer, len                    :: n3
      integer(k3)                     :: x = -999
      class(inner(:,k3)), allocatable :: i
      contains
         procedure :: badd
         generic :: operator (+) => badd
   end type

   interface
      type(base(20,4)) function badd (a, b)
         import base
         class(base(*,4)), intent(in) :: a,b
      end function
   end interface

   contains

      class(inner(20,4)) function iadd ( a, b ) result(abc)
         class(inner(*,4)), intent(in) :: a, b
         allocatable :: abc

         allocate ( abc, source = inner(20,4) ( j = a%j + b%j ) )
      end function

      class(inner(20,4)) function icadd ( a, b ) result(abc)
         class(innerc(*,4,*,4)), intent(in) :: a
         class(inner(*,4)), intent(in) ::  b
         allocatable :: abc

         select type ( b )
            type is ( innerc(*,4,*,4) )
               allocate ( abc, source = innerc(20,4,20,4) ( j = a%j + b%j, k = a%k + b%k ) )
         end select

      end function

end module

type(base(20,4)) function badd (a, b)
   use m, only: base
   class(base(*,4)), intent(in) :: a,b

   badd%x = a%x + b%x
   allocate ( badd%i, source = a%i + b%i )

end function

program genericOperatorStructConstr005
   use m

   class(base(:,4)), allocatable :: b1
   class(base(:,4)), pointer :: b2

   type(base(20,4)) :: b3

   allocate ( b1, source = base(20,4) ( x = 10, i = null() ) )
   allocate ( b2, source = base(20,4) ( x = 30, i = innerc(20,4,20,4)(j = 300, k = 3000) ) + base(20,4) ( x = 40, i = innerc(20,4,20,4)( j = 400, k = 4000) ) )

   allocate ( b1%i, source = innerc(20,4,20,4)(j = 100, k = 1000) + innerc(20,4,20,4)( j = 200, k = 2000) )

   select type ( g => b1%i )
      type is ( innerc(*,4,*,4) )
         if ( ( b1%x /= 10 ) .or. ( g%j /= 300 ) .or. ( g%k /= 3000 ) ) error stop 1_4
      class default
         error stop 2_4
   end select

   select type ( g => b2%i )
      type is ( innerc(*,4,*,4) )
         if ( ( b2%x /= 70 ) .or. ( g%j /= 700 ) .or. ( g%k /= 7000 ) ) error stop 3_4
      class default
         error stop 4_4
   end select

   b3 = base(20,4) ( x = 50, i = innerc(20,4,20,4) ( 500, 5000 ) ) + base(20,4) ( x = 60, i = innerc(20,4,20,4) ( 600, 6000 ) )

   select type ( g => b3%i )
      type is ( innerc(*,4,*,4) )
         if ( ( b3%x /= 110 ) .or. ( g%j /= 1100 ) .or. ( g%k /= 11000 ) ) error stop 5_4
      class default
         error stop 6_4
   end select

end
