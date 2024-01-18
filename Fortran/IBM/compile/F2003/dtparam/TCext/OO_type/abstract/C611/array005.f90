! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C611/array005.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        polymorphic abstract type data-ref assigned data, allocate statement
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

   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      real(k2)      :: rid
   end type

   type :: otherbase(k3,n1)    ! (4,20)
      integer, kind                              :: k3
      integer, len                               :: n1
      class(base(k3)), dimension(:), allocatable :: ptr
   end type

   type, extends(otherbase) :: otherchild(k4,n2)    ! (4,20,4,20)
       integer, kind :: k4
       integer, len  :: n2
   end type


end module

program array005
   use m

   type(child(4,4)), target :: c1(5)
   type(otherbase(4,20)) :: ob1
   class(otherbase(4,20)), pointer :: ob2
   type(otherchild(4,20,4,20)), allocatable, target :: oc1

   allocate (oc1)
   ob2 => oc1


   allocate(ob1%ptr(5), source = c1 )
      deallocate(ob1%ptr)
   allocate(ob1%ptr(5), source = c1%base )
      deallocate(ob1%ptr)
   allocate(ob1%ptr(3), source = c1(1:3)%base )
      deallocate(ob1%ptr)
   allocate(ob1%ptr(0), source = c1(1:0)%base )
      deallocate(ob1%ptr)

   allocate(ob2%ptr(5), source = c1 )
      deallocate(ob2%ptr)
   allocate(ob2%ptr(5), source = c1%base )
      deallocate(ob2%ptr)
   allocate(ob2%ptr(3), source = c1(1:3)%base )
      deallocate(ob2%ptr)
   allocate(ob2%ptr(0), source = c1(1:0)%base )
      deallocate(ob2%ptr)

   allocate(oc1%ptr(5), source = c1 )
      deallocate(oc1%ptr)
   allocate(oc1%ptr(5), source = c1%base )
      deallocate(oc1%ptr)
   allocate(oc1%ptr(3), source = c1(1:3)%base )
      deallocate(oc1%ptr)
   allocate(oc1%ptr(0), source = c1(1:0)%base )
      deallocate(oc1%ptr)



end program
