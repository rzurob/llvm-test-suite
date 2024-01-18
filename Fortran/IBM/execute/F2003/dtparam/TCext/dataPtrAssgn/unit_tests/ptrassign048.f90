! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign048.f
! opt variations: -qnol -qnodeferredlp

!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign048.f
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

  type dt1(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)   :: data
  end type

  type(dt1(:,4)), pointer :: ptr1(:), ptr2(:)

  type(dt1(:,4)), allocatable,target :: tar(:)

  integer :: lowerb

  allocate(tar(20), source=(/(dt1(20,4)(i),i=1,20)/))

  lowerb=10

  ptr1(lowerb:)=>tar(5:14)

  if(lbound(ptr1, dim=1).ne. 10) error stop 1
  if(ubound(ptr1, dim=1).ne. 19) error stop 2
  if(any(shape(ptr1).ne.(/10/))) error stop 3
  if(.not.associated(ptr1,tar(5:14))) error stop 4

  lowerb=-5
  ptr2(lowerb:)=>ptr1

  if(lbound(ptr2, dim=1).ne. -5) error stop 5
  if(ubound(ptr2, dim=1).ne. 4) error stop 6
  if(any(shape(ptr2).ne.(/10/))) error stop 7
  if(.not.associated(ptr2,ptr1)) error stop 8

end


