! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign011.f
! opt variations: -qnol -qnodeferredlp

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign011.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2011
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=polymorphic
!*
!*  DESCRIPTION                :C716 functional
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  type dt1(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)   :: num1
  end type

  type, extends(dt1) :: dt1ext    ! (20,4)
  end type


  class(dt1(:,4)), pointer :: ptr1(:)
  class(dt1ext(:,4)), pointer :: ptr2(:)
  type(dt1ext(20,4)), target :: arr1(1:10)


  ptr2(4:)=>arr1
  if(.not.associated(ptr2,arr1)) error stop 1
  ptr1(5:)=>ptr2(4:)
  if(.not.associated(ptr1,arr1)) error stop 2
  nullify(ptr1)
  ptr1(6:15)=>arr1
  if(.not.associated(ptr1,arr1)) error stop 3


end

