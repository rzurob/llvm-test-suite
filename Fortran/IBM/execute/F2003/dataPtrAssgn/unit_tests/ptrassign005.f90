!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign072.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
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
      integer :: data
      contains
        procedure, nopass :: assign=>ptrassign
    end type
    contains

      subroutine ptrassign(ptr, tar)
        type(base), pointer :: ptr(:,:), ptr2
        type(base), target  :: tar(:)
        integer :: num=1

        ptr(4:7,10:19)=>tar

        if(lbound(ptr, dim=1).ne. 4) error stop 1
        if(lbound(ptr, dim=2).ne. 10) error stop 2
        if(ubound(ptr, dim=1).ne. 7) error stop 3
        if(ubound(ptr, dim=2).ne. 19) error stop 4
        if(any(shape(ptr).ne.(/4,10/))) error stop 5
        do i=10,19
          do j=4,7
             ptr2=>ptr(j,i)
             if(.not.associated(ptr2,tar(num))) error stop 6
             num=num+1
           end do
         end do

      end subroutine


  end module

  use m

  type(base), pointer :: ptr1(:,:), ptr2
  type(base), target, allocatable  :: tar1(:)
  integer :: num=1

  allocate(tar1(45), source=(/(base(i),i=1,45)/))

  call ptr1%assign(ptr1,tar1)

  if(lbound(ptr1, dim=1).ne. 4) error stop 7
  if(lbound(ptr1, dim=2).ne. 10) error stop 8
  if(ubound(ptr1, dim=1).ne. 7) error stop 9
  if(ubound(ptr1, dim=2).ne. 19) error stop 10
  if(any(shape(ptr1).ne.(/4,10/))) error stop 11

  do i=10,19
    do j=4,7
      ptr2=>ptr1(j,i)
      if(.not.associated(ptr2,tar1(num))) error stop 6
       num=num+1
     end do
   end do




end