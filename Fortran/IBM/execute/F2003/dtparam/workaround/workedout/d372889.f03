! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2009-12-10
!*
!*  DESCRIPTION                : defect 372889
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type A(k1,l1)
      integer,kind :: k1=2
      integer,len  :: l1=2

      sequence
      character(l1) :: c1="**"
      character(k1) :: c2(l1)="##"
      integer(k1)   :: i1=-99
   end type

   type B(k2,l2)
      integer,kind :: k2=2
      integer,len  :: l2=4

      sequence
      logical(k2)   :: g1(l2-1:l2,l2:l2+1)=.false.
      integer(2*k2) :: i2(l2-1:l2,1)=-99
   end type

   type C(k3,l3)
       integer,kind :: k3=2
       integer,len  :: l3=3

       sequence
       type(A(k3,l3-1)) :: a1comp(l3:l3+1)
       type(B(k3,l3+1)) :: b1comp
   end type

   contains

       subroutine set_auto(dt,obj2)
          type(C(2,*)),intent(in) :: dt(:)
          type(C(2,dt%l3)),allocatable :: obj2(:)

          obj2=dt
       end subroutine
end module
end
