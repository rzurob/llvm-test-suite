! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorScalar009.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar with polymorphic inner type within types
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

   type innerbase(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         generic, private :: operator(+) => inneradd
         procedure :: inneradd
   end type

   type, extends(innerbase) :: innerchild    ! (20,4)
      integer(k1) :: j
      contains
         procedure :: inneradd => innercadd
   end type

   type base(k2,n2)    ! (4,20)
      integer, kind                       :: k2
      integer, len                        :: n2
      class(innerbase(:,k2)), allocatable :: b1
      contains
         generic :: operator(+) => add
         procedure :: add
   end type

   contains

   class(innerbase(:,4)) function inneradd ( a, b )
      intent(in) :: a, b
      class(innerbase(*,4)) :: a, b
      allocatable :: inneradd

      allocate (innerbase(20,4):: inneradd)

      inneradd%i = a%i + b%i

   end function

   type(base(4,20)) function add ( a, b )
      intent(in) :: a, b
      class(base(4,*)) :: a, b
      if ( allocated ( add%b1 ) ) deallocate ( add%b1 )
      allocate ( add%b1, source = a%b1 + b%b1 )
   end function

   class(innerbase(:,4)) function innercadd ( a, b )
      intent(in) :: a, b
      class(innerchild(*,4)) :: a
      class(innerbase(*,4)) b

      allocatable :: innercadd

      select type ( b )
         type is ( innerchild(*,4) )
            allocate ( innercadd, source = innerchild(20,4) ( innerbase = ( a%innerbase + b%innerbase ), j = a%j + b%j ) )
         class default
            error stop 10_4
      end select

   end function

end module

program genericOperatorScalar009
   use m

   class(base(4,:)), allocatable :: b1
   class(base(4,:)), allocatable :: b2, b3

   allocate ( b1, source = base(4,20) ( innerbase(20,4) (10) ) )
   allocate ( b2, source = b1 + base(4,20) ( innerbase(20,4) (10) ) )

   allocate ( b3, source = b1 + b2 )

   if ( b1%b1%i /= 10 ) error stop 1_4
   if ( b2%b1%i /= 20 ) error stop 2_4
   if ( b3%b1%i /= 30 ) error stop 3_4

   deallocate ( b1, b2, b3 )

   allocate ( b1, source = base(4,20) ( innerchild(20,4) (10,100) ) )
   allocate ( b2, source = b1 + base(4,20) ( innerchild(20,4) (20,200) ) )

   allocate ( b3, source = b1 + b2 )

   select type ( g=> b1%b1 )
      type is ( innerchild(*,4) )
         if ( ( g%i /= 10 ) .or. ( g%j /= 100 ) )  error stop 4_4
      class default
         error stop 5_4
   end select

   select type ( g=>b2%b1 )
      type is ( innerchild(*,4) )
         if ( (g%i /= 30 ) .or. (g%j /= 300 ) )  error stop 6_4
      class default
         error stop 7_4
   end select

   select type ( g=> b3%b1 )
      type is ( innerchild(*,4) )
         if ( ( g%i /= 40 ) .or. ( g%j /= 400 ) )  error stop 8_4
      class default
         error stop 9_4
   end select

end program
