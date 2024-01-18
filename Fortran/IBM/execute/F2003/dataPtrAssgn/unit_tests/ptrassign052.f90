!****************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :functional testing of bounds-remapping and bounds-spec
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

   type base
     real, pointer :: ptr(:,:,:)
   end type

   contains

     function func1(tar)
       real, target :: tar(:)
       type(base), save :: b1, b2
       integer :: func1


       if(.not.associated(b1%ptr)) then
         b1%ptr(11:15,11:15,11:12)=>tar
       end if

       if(.not.associated(b2%ptr)) then
         b2%ptr(21:25,21:25,21:22)=>tar
       end if

       if(lbound(b1%ptr, dim=1).ne. 11) error stop 9
       if(lbound(b1%ptr, dim=2).ne. 11) error stop 10
       if(lbound(b1%ptr, dim=3).ne. 11) error stop 11
       if(ubound(b1%ptr, dim=1).ne. 15) error stop 12
       if(ubound(b1%ptr, dim=2).ne. 15) error stop 13
       if(ubound(b1%ptr, dim=3).ne. 12) error stop 14
       if(any(shape(b1%ptr).ne.(/5,5,2/))) error stop 15


       if(lbound(b2%ptr, dim=1).ne. 21) error stop 17
       if(lbound(b2%ptr, dim=2).ne. 21) error stop 18
       if(lbound(b2%ptr, dim=3).ne. 21) error stop 19
       if(ubound(b2%ptr, dim=1).ne. 25) error stop 20
       if(ubound(b2%ptr, dim=2).ne. 25) error stop 21
       if(ubound(b2%ptr, dim=3).ne. 22) error stop 22
       if(any(shape(b2%ptr).ne.(/5,5,2/))) error stop 23


       func1=0

     end function

end module

program main
  use m

  type(base) :: b1
  real, target :: tar(50)

  interface
    subroutine sub1(tar)
      real, target :: tar(:)
    end subroutine
  end interface


  do i=1,50
    tar(i)=real(i)
  end do


  call sub1(tar)
  call sub1(tar)

  i=func1(tar)
  i=func1(tar)

end program

subroutine sub1(tar)
  use m
  real, target :: tar(:)
  type(base), save :: b1


  if(.not.associated(b1%ptr)) then
    b1%ptr(1+2+2:3**2,int(35/7):3**2,int(sqrt(4.0)):3)=>tar
  end if

  if(lbound(b1%ptr, dim=1).ne. 5) error stop 1
  if(lbound(b1%ptr, dim=2).ne. 5) error stop 2
  if(lbound(b1%ptr, dim=3).ne. 2) error stop 3
  if(ubound(b1%ptr, dim=1).ne. 9) error stop 4
  if(ubound(b1%ptr, dim=2).ne. 9) error stop 5
  if(ubound(b1%ptr, dim=3).ne. 3) error stop 6
  if(any(shape(b1%ptr).ne.(/5,5,2/))) error stop 7

end subroutine

