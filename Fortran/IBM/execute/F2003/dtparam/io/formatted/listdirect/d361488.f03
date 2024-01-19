!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 26 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  defect 361488
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type inner3(l3)
      integer,len  :: l3 !l3=3
      real         :: r1(l3:l3+1)=-9.9
   end type
  type outer(l4)
     integer,len  :: l4 ! l4=4
     complex(8)   :: x1(l4-1)=(9.9_8,-9.9_8)
     type(inner3(l4-1)) :: inn3
  end type
end module

program d361488

  use m
  type(outer(:)),allocatable :: outobj2
  logical,external :: precision_r4,precision_x6

  allocate(outer(4) :: outobj2)

  open(10,file='d361488.dat')
  read(10,*) outobj2


  if(.not. precision_x6(outobj2%x1(1),(9.9_8,-9.9_8) )) error stop 1
  if(.not. precision_x6(outobj2%x1(2),(3.4_8,-7.8_8) )) error stop 2
  if(.not. precision_x6(outobj2%x1(3),(0.5_8,-0.5D-2))) error stop 3

  if(.not. precision_r4(outobj2%inn3%r1(3),-3.4 ))      error stop 4
  if(.not. precision_r4(outobj2%inn3%r1(4),-2.3E-3 ))   error stop 5


  close(10)

end program
