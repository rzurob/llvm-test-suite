!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 24 2009
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!*  defect 361393
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1,l1)
      integer,kind  :: k1=4
      integer,len   :: l1=2

   end type

   type,extends(base) :: child(k2,l2)
      integer,kind  :: k2=8
      integer,len   :: l2=4

      real(k2)      :: r1(l1:l2)=1.
   end type

   type,extends(child) :: gen3(k3,l3)
      integer,kind :: k3=4
      integer,len  :: l3=5

      logical(k3) :: g1(l1:l2-1,l2:l3)=.false.
   end type

end module
   use m
   class(base(4,:)),allocatable:: t1
   logical,external :: precision_r8

   allocate(gen3(4,2,8,4,2,5) :: t1)
   select type(t1)
      type is(gen3(4,*,8,*,2,*))

        if(t1%g1(2,4) .neqv. .false.)          stop 1
        if(.not. precision_r8(t1%r1(2),1._8))  stop 2

      class default
        stop 3
   end select

end program
