! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign051.f
! opt variations: -qnol

!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign051.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : ptrassign051
!*
!*  PROGRAMMER                 : Michael Selvanayagam
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                :functional testing of bounds-remapping and bounds-spec
!*                              
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

   type base(n1,k1)    ! (20,4)
     integer, kind        :: k1
     integer, len         :: n1
     complex(k1), pointer :: ptr(:,:,:)
   end type
   
   contains
     
     subroutine sub2(tar)
       complex, target :: tar(:,:,:)
       type(base(20,4)), save :: b1, b2
       
       if(.not.associated(b1%ptr)) then
         b1%ptr(10:,15:,20:)=>tar
       end if
       
       if(.not.associated(b2%ptr)) then 
         b2%ptr(-10:,-15:,-20:)=>b1%ptr
       end if
  
       if(lbound(b1%ptr, dim=1).ne. 10) error stop 9
       if(lbound(b1%ptr, dim=2).ne. 15) error stop 10
       if(lbound(b1%ptr, dim=3).ne. 20) error stop 11
       if(ubound(b1%ptr, dim=1).ne. 19) error stop 12
       if(ubound(b1%ptr, dim=2).ne. 29) error stop 13
       if(ubound(b1%ptr, dim=3).ne. 39) error stop 14
       if(any(shape(b1%ptr).ne.(/10,15,20/))) error stop 15
       if(.not.associated(b1%ptr,tar)) error stop 16
       
       if(lbound(b2%ptr, dim=1).ne. -10) error stop 17
       if(lbound(b2%ptr, dim=2).ne. -15) error stop 18
       if(lbound(b2%ptr, dim=3).ne. -20) error stop 19
       if(ubound(b2%ptr, dim=1).ne. -1) error stop 20
       if(ubound(b2%ptr, dim=2).ne. -1) error stop 21
       if(ubound(b2%ptr, dim=3).ne. -1) error stop 22
       if(any(shape(b2%ptr).ne.(/10,15,20/))) error stop 23
       if(.not.associated(b2%ptr,tar)) error stop 24
       
     end subroutine
   
end module

program main
  use m
  
  type(base(20,4)) :: b1
  complex, target :: tar(10,15,20)
  
  interface
    subroutine sub1(tar)
      complex, target :: tar(:,:,:)
    end subroutine
  end interface
  
  do i=1,20
    do j=1,15
      do k=1,10
        tar(k,j,i)=(k,j)
      end do
    end do
  end do

  call sub1(tar)
  call sub1(tar)
  
  call sub2(tar)
  call sub2(tar)
  
end program

subroutine sub1(tar)
  use m
  complex, target :: tar(10,15,20)
  type(base(20,4)), save :: b1
  
  if(.not.associated(b1%ptr)) then
    b1%ptr(10:,15:,20:)=>tar
  end if
  
  if(lbound(b1%ptr, dim=1).ne. 10) error stop 1
  if(lbound(b1%ptr, dim=2).ne. 15) error stop 2
  if(lbound(b1%ptr, dim=3).ne. 20) error stop 3
  if(ubound(b1%ptr, dim=1).ne. 19) error stop 4
  if(ubound(b1%ptr, dim=2).ne. 29) error stop 5
  if(ubound(b1%ptr, dim=3).ne. 39) error stop 6
  if(any(shape(b1%ptr).ne.(/10,15,20/))) error stop 7
  if(.not.associated(b1%ptr,tar)) error stop 8
end subroutine

