! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign032.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign032.f
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

  class(base(:,4)), allocatable, target :: tar1(:,:)

  class(base(:,4)), pointer :: ptr(:,:)

  allocate(base(20,4) :: tar1(5,6))


  ptr(2:,3:)=>tar1(1:4,2:5)

  select type (ptr)
    type is (base(*,4))

     if(lbound(ptr, dim=1).ne. 2) error stop 1
     if(lbound(ptr, dim=2).ne. 3) error stop 2
     if(ubound(ptr, dim=1).ne. 5) error stop 3
     if(ubound(ptr, dim=2).ne. 6) error stop 4
     if(any(shape(ptr).ne.(/4,4/))) error stop 5


   class default
     error stop 7
  end select

  if(.not.associated(ptr,tar1(1:4,2:5))) error stop 6


end
