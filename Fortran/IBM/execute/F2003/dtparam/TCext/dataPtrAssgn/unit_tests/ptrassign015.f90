! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign015.f
! opt variations: -ql -qreuse=none

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
!*  DESCRIPTION                :functional testing of bound-spec
!*
!234567890123456789012345678901234567890123456789012345678901234567890

	type ptrtype(k1)    ! (4)
     integer, kind     :: k1
     real(k1), pointer :: ptr1(:)
     real(k1), pointer :: ptr2(:,:,:)
  end type

  type tartype(k2,n1,n2)    ! (4,1,10)
    integer, kind :: k2
    integer, len  :: n1,n2
    real(k2)      :: tar1(n1:n2)
    real(k2)      :: tar2(n1:n2, n1+1:n2+1, n1+2:n2+2)
  end type

  type(ptrtype(4)) :: ptr

  type(tartype(4,1,10)), target :: tar

  integer :: num1,num2, num3

  num1=10
  num2=20
  num3=30

  ptr%ptr1((25*5)/(5**2):)=>tar%tar1

  if(lbound(ptr%ptr1, dim=1) .ne. 5)  error stop 1
  if(ubound(ptr%ptr1, dim=1) .ne. 14)  error stop 2
  if(any(shape(ptr%ptr1) .ne. shape(tar%tar1)))  error stop 3
  if(.not.associated(ptr%ptr1,tar%tar1))  error stop 4

  ptr%ptr2(num1:,num2:,num3:)=>tar%tar2

  if(lbound(ptr%ptr2, dim=1) .ne. num1)  error stop 5
  if(lbound(ptr%ptr2, dim=2) .ne. num2)  error stop 6
  if(lbound(ptr%ptr2, dim=3) .ne. num3)  error stop 7
  if(ubound(ptr%ptr2, dim=1) .ne. num1+9)  error stop 8
  if(ubound(ptr%ptr2, dim=2) .ne. num2+9)  error stop 9
  if(ubound(ptr%ptr2, dim=3) .ne. num3+9)  error stop 10
  if(.not.associated(ptr%ptr2,tar%tar2))  error stop 11
  if(any(shape(ptr%ptr2) .ne. shape(tar%tar2)))  error stop 12
  if(.not.associated(ptr%ptr2,tar%tar2))  error stop 13

end

