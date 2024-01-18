! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign045.f
! opt variations: -ql

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign045.f
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
!*  TEST CASE TITLE            : ptrassign045
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

  type dtptr(k1)    ! (4)
    integer, kind      :: k1
    real(k1) , pointer :: ptr(:,:)
  end type
  
  type dttar(k2)    ! (4)
    integer, kind :: k2
    real(k2)      :: tar(50)=(/(real(i), i=1,50)/)
  end type
  
  type(dttar(4)) :: arr1
  
  call sub1(arr1%tar)
   
   contains
  
    subroutine sub1(tar)
      real, target :: tar(:)
      
      type(dtptr(4)) :: dtptr1
      
      real, pointer :: ptr2
      
      integer :: num=2
      
      dtptr1%ptr(3:4,5:9)=>tar(2:11)
      
      if(lbound(dtptr1%ptr, dim=1).ne. 3) error stop 1
      if(lbound(dtptr1%ptr, dim=2).ne. 5) error stop 2
      if(ubound(dtptr1%ptr, dim=1).ne. 4) error stop 3
      if(ubound(dtptr1%ptr, dim=2).ne. 9) error stop 4
      if(any(shape(dtptr1%ptr).ne.(/2,5/))) error stop 5
      
      do i=5,9
        do j=3,4
          ptr2=>dtptr1%ptr(j,i)
          if(.not.associated(ptr2,tar(num))) error stop 6
          num=num+1
        end do
      end do
    end subroutine
    
end
