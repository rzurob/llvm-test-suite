! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign031.f
! opt variations: -ql -qreuse=none

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign031.f
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

  type base(k1)    ! (4)
    integer, kind :: k1
    integer(k1)   :: num1
  end type

  type ,extends(base) :: child    ! (4)
    integer(k1) :: num2
  end type

  type(child(4)), target :: tar1(25)=(/(child(4)(i,i),i=1,25)/)

  class(base(4)), pointer :: ptr(:)

  ptr(25:)=>tar1

  select type (ptr)
    type is (child(4))

     if(lbound(ptr, dim=1).ne. 25) error stop 1
     if(ubound(ptr, dim=1).ne. 49) error stop 2
     if(any(shape(ptr).ne.(/25/))) error stop 3

   class default
     error stop 7
  end select

  if(.not.associated(ptr,tar1)) error stop 4


end

