! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorStructConstr006.f
! opt variations: -qnok -qnol -qnodeferredlp

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
!*  DESCRIPTION                : Operator: Scalar to Scalar structure constructor w/ unlimited polymorphic components
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module x

   type inner(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure, pass :: isub
         generic :: operator(-) => isub
   end type

   interface operator (-)
      module procedure intsub
   end interface operator(-)

   contains

   type(inner(20,4)) function isub ( a, b )
      class(inner(*,4)), intent(in) :: a
      class(*), intent(in) :: b

      select type ( b )
         type is (integer )
            isub%i = a%i - b
         type is ( inner(*,4) )
            isub%i = a%i - b%i
      end select

   end function

   integer function intsub ( a, b )
      integer, intent(in) :: a
      type(inner(*,4)), intent(in) :: b

      intsub = a - b%i

   end function

end module


module m
   use x

   type base(k2,n2)    ! (4,20)
       integer, kind :: k2
       integer, len  :: n2
      class(*), pointer :: u1
      class(*), allocatable :: u2
      contains
         procedure, pass :: bsub
         generic, public :: operator (-) => bsub
   end type

   contains

   type(base(4,20)) function bsub ( a, b )
      class(base(4,*)), intent(in) :: a, b

      select type ( g => a%u1 )
         type is (inner(*,4))
            allocate ( bsub%u1 , source = g - b%u1 )
         type is (integer)
            select type ( h => b%u1 )
               type is ( inner(*,4) )
                  allocate ( bsub%u1 , source = g - h )
               type is ( integer )
                  allocate ( bsub%u1 , source = g - h )
            end select
      end select

      select type ( g => a%u2 )
         type is (inner(*,4))
            allocate ( bsub%u2 , source = g - b%u2 )
         type is (integer)
            select type ( h => b%u2 )
               type is ( inner(*,4) )
                  allocate ( bsub%u2 , source = g - h )
               type is ( integer )
                  allocate ( bsub%u2 , source = g - h )
            end select
      end select

   end function

end module


program genericOperatorStructConstr006
   use m

   type (base(4,20)) :: b1
   type(base(4,:)), allocatable :: b2
   type(inner(20,4)), target :: i1, i2

   integer, target :: i11, i12

   i1 = inner(20,4) (10)
   i2 = inner(20,4)(30)
   allocate ( base(4,20):: b2 )

   b2 = base(4,20) ( i2, inner(20,4) (40_4) ) - base(4,20) ( i1 , 20_4 )

   select type ( g=>b2%u1 )
      type is ( inner(*,4) )
         print *, g
   end select

   select type ( g=>b2%u2 )
      type is ( inner(*,4) )
         print *, g
   end select

   b1 = base(4,20) ( i2, inner(20,4) (40_4) ) - base(4,20) ( i1 , 20_4 ) - base(4,20) ( i1 , -20_4 )

   select type ( g=>b1%u1 )
      type is ( inner(*,4) )
         print *, g
   end select

   select type ( g=>b1%u2 )
      type is ( inner(*,4) )
         print *, g
   end select

   i11 = 100
   i12 = 200

   deallocate ( b2 )
   allocate ( base(4,20):: b2 )

   b2 = base(4,20) ( i12, 300 ) - base(4,20) ( i12, 300 ) - base(4,20) ( i12, 300 )

   select type ( g=>b2%u1 )
      type is ( integer )
         print *, g
   end select

   select type ( g=>b2%u2 )
      type is ( integer )
         print *, g
   end select

   b1 = base(4,20) ( i2, i1 ) - base(4,20) ( i1, i2 )
   select type ( g=>b1%u1 )
      type is ( inner(*,4) )
         print *, g
   end select

   select type ( g=>b1%u2 )
      type is ( inner(*,4) )
         print *, g
   end select

end
