! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign021.f
! opt variations: -qnol -qnodeferredlp

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign021.f
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
!*  TEST CASE TITLE            : ptrassign021
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

  type dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)   :: num
  end type
  
  integer :: num1=1
  
  type(dt(:,4)), pointer :: ptr1(:,:), ptr2(:), ptr3
  
  type(dt(20,4)), target :: tar1(10)=(/(dt(20,4)(i),i=1,10)/)
  
  ptr1(bound(10):bound(11),bound(15):bound(19))=>tar1
  
  ptr2(bound(5):)=>tar1
  
  if(lbound(ptr1, dim=1).ne. 10) error stop 1
  if(lbound(ptr1, dim=2).ne. 15) error stop 2
  if(ubound(ptr1, dim=1).ne. 11) error stop 3
  if(ubound(ptr1, dim=2).ne. 19) error stop 4
  if(any(shape(ptr1).ne.(/2,5/))) error stop 5
  
  do i=15,19
    do j=10,11
      ptr3=>ptr1(j,i)
      if(.not.associated(ptr3,tar1(num1))) error stop 7
      num1=num1+1
    end do
  end do

  
  if(lbound(ptr2, dim=1) .ne. 5)  error stop 8
  if(ubound(ptr2, dim=1) .ne. 14)  error stop 9
  if(any(shape(ptr2) .ne. shape(tar1)))  error stop 10
  if(.not.associated(ptr2,tar1))  error stop 11
  


  contains
  
    function bound(num2)
      integer :: bound
      integer :: num2
      
      bound=num2
      
    end function
    
end
      
