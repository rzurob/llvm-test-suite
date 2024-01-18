! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorPass003.f
! opt variations: -qnok -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Binary Operator: with pass attribute with binding defined for child types
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i = -999
      contains
         procedure, pass(b) :: base_base_base
         procedure, pass(b) :: base_child_base
         generic :: operator(*) => base_child_base, base_base_base
   end type

   type, extends(base) :: child(k2,n2)    ! (20,4,4,20)
       integer, kind :: k2
       integer, len  :: n2
   end type

   contains

   function base_base_base ( a, b )
      type(base(*,4)), intent(in) :: a
      class(base(*,4)), intent(in) :: b

      class(base(:,4)), allocatable :: base_base_base

      allocate ( base_base_base, source = b )

      base_base_base%i = a%i * base_base_base%i
      print *, 'base_base_base'

   end function

   function base_child_base ( a, b )
      type(child(*,4,4,*)), intent(in) :: a
      class(base(*,4)), intent(in) :: b

      class(base(:,4)), allocatable :: base_child_base

      allocate ( base_child_base, source = b )

      base_child_base%i = a%i * base_child_base%i
      print *, 'base_child_base'

   end function

end module

program genericOperatorPass003
   use m

   class(base(:,4)), allocatable :: b1, b2, b3
   allocate ( b1, source = base(20,4)(5) * base(20,4)(4) )

   allocate ( b2, source = b1 * base(20,4)(10) )

   print *, b1%i
   print *, b2%i

   allocate ( b3, source = b1 * b2 * b1 * b2 )
   print *, b3%i

   deallocate ( b3 )

   allocate ( b3, source = child(20,4,4,20)(2) * b1 * b2 * child(20,4,4,20)(3) )

   select type ( b3 )
      type is ( child(*,4,4,*) )
         print *, b3%i
   end select

   deallocate ( b1 )

   allocate ( b1, source = child(20,4,4,20)(10) * child(20,4,4,20)(20) * b3 )
   print *, b1%i

end program
