!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 27 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  DEFECT 359481
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dt(l)
      integer(2),len :: l
      character(l)   :: c
   end type
   type base(l1)
       integer,len   :: l1
       type(dt(2*l1))  :: dtcomp1
   end type

   type,extends(base) :: child(l2)
       integer,len    :: l2
       type(dt(l2)) :: dtcomp2
   end type

end module

program d359481

  use m
  implicit none

  type(child(:,:)),allocatable :: child1

  allocate(child1,source=child(1,5)(dtcomp1=dt(2)("01"), &
                    dtcomp2=dt(5)("23456") ) )
  print *,child1%dtcomp1%l,child1%dtcomp2%l,child1%dtcomp1%c

end program
