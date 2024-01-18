! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign028.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign028.f
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

  type base(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)   :: num1
  end type

  type ,extends(base) :: child    ! (20,4)
    integer(k1) :: num2
  end type

  integer :: num=1

  class(base(:,4)), allocatable, target :: tar1(:)

  class(*), pointer :: ptr1(:,:), ptr2, ptr3(:,:)

  allocate(tar1(30),source=(/(child(20,4)(i,i),i=1,30)/))


  ptr1(1:4,1:5)=>tar1

  select type (ptr1)
    type is (child(*,4))

     if(lbound(ptr1, dim=1).ne. 1) error stop 1
     if(lbound(ptr1, dim=2).ne. 1) error stop 2
     if(ubound(ptr1, dim=1).ne. 4) error stop 3
     if(ubound(ptr1, dim=2).ne. 5) error stop 4
     if(any(shape(ptr1).ne.(/4,5/))) error stop 5

     do i=1,5
       do j=1,4
           ptr2=>ptr1(j,i)
           if(.not.associated(ptr2,tar1(num))) error stop 6
           num=num+1
       end do
     end do
   class default
     error stop 7
  end select

  ptr3(6:,6:)=>ptr1(1:4,1:5)

  select type (ptr3)
    type is (child(*,4))

     if(lbound(ptr3, dim=1).ne. 6) error stop 8
     if(lbound(ptr3, dim=2).ne. 6) error stop 9
     if(ubound(ptr3, dim=1).ne. 9) error stop 10
     if(ubound(ptr3, dim=2).ne. 10) error stop 11
     if(any(shape(ptr3).ne.(/4,5/))) error stop 12
     num=1
     do i=6,10
       do j=6,9
           ptr2=>ptr3(j,i)
           if(.not.associated(ptr2,tar1(num))) error stop 13
           num=num+1
       end do
     end do
   class default
     error stop 14
  end select


end
