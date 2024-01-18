! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign001.f
! opt variations: -qnol -qreuse=none

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign001.f
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
!*  DESCRIPTION                :testing syntax -functional
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  integer :: num1, num2
  integer , pointer :: ptr1(:), ptr2(:,:)

  integer, target :: arr1(1:10), arr2(1:10,5:14)

  type ptrtype(n1,k1)    ! (20,4)
    integer, kind        :: k1
    integer, len         :: n1
    integer(k1), pointer :: ptr3(:)
    integer(k1), pointer :: ptr4(:,:)
  end type

  type(ptrtype(20,4)) :: ptrtype1

  !bounds-spec-list
  ptr1(2:)=>arr1

  ptr1(num1:)=>arr1

  ptr2(3:,4:)=>arr2

  ptrtype1%ptr3(5:)=>arr1

  ptrtype1%ptr4(6:, 7:)=>arr2

  ptrtype1%ptr4(num1:, num2:)=>arr2

  !bounds-remapping-list

  ptr1(5:14)=>arr1

  ptr1(num1:num2)=>arr1

  ptr2(5:14,19:19)=>arr1

  ptr2(1:5,6:7)=>arr1

  ptrtype1%ptr3(6:15)=>arr1

  ptrtype1%ptr4(6:15, 7:7)=>arr1

  ptrtype1%ptr4(11:15,16:17)=>arr1

  ptrtype1%ptr4(num1:num2,num1:num2)=>arr1


end

