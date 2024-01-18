! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign009d.f
! opt variations: -qnol -qnodeferredlp

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign010.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=polymorphic
!*
!*  DESCRIPTION                :C716 - diagnostic
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  type dt1(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)   :: num1
  end type

  type, extends(dt1) :: dt1ext    ! (20,4)
  end type

  type dt2(n2,k2)    ! (20,4)
    integer, kind :: k2
    integer, len  :: n2
    complex(k2)   :: num2
  end type

  type, extends(dt2) :: dt2ext    ! (20,4)
  end type



  class(dt1(:,4)), pointer :: ptr1(:)
  type(dt1(20,4)), target :: arr1(1:10)
  class(dt1ext(:,4)), pointer :: ptr2(:)

  class(dt2(:,4)), pointer :: ptr3(:)
  type(dt2(20,4)), target :: arr2(1:10)
  class(dt2ext(:,4)), pointer :: ptr4(:)

  ptr1(5:)=>arr2
  ptr2(10:19)=>arr1
  ptr2(10:10)=>arr2

  ptr3(4:)=>arr1
  ptr4(5:14)=>arr2
  ptr4(5:14)=>arr1

end

