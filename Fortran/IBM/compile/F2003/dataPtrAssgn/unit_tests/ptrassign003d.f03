!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qlanglvl=90std
!*
!*  DESCRIPTION                :testing syntax -diagnostic, langlvl
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  complex , pointer :: ptr1(:), ptr2(:,:)

  complex, target :: arr1(1:10), arr2(1:10,5:14)

  type ptrtype
    complex, pointer :: ptr3(:)
    complex, pointer :: ptr4(:,:)
  end type

  type(ptrtype) :: ptrtype1

  !bounds-spec-list
  ptr1(2:)=>arr1

  ptr2(3:,4:)=>arr2

  ptrtype1%ptr3(5:)=>arr1

  ptrtype1%ptr4(6:, 7:)=>arr2

  !bounds-remapping-list
  ptr1(5:14)=>arr1

  ptr2(5:14,19:19)=>arr1

  ptr2(1:5,6:7)=>arr1

  ptrtype1%ptr3(6:15)=>arr1

  ptrtype1%ptr4(6:15, 7:7)=>arr1

  ptrtype1%ptr4(11:15,16:17)=>arr1


end
