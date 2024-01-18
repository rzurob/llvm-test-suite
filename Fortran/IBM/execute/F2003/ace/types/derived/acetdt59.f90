!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt59
!*
!*  DATE                       : 2006-11-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : self-referential assignment, including array sections (derived)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Repeat the integer tests on array sections with a derived type.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt59mod

  implicit none
  type item
     integer :: val
  end type item

  type derived
     type (item) :: ivals(5)
     type (item), allocatable :: iavals(:)
  end type derived

end module acetdt59mod


program acetdt59

  use acetdt59mod
  implicit none
  type (item) :: iarr(5)
  integer     :: inx(5), i
  type (item), allocatable :: iarra(:)
  type (derived) :: dt

  iarr = [item:: (item(i ** 2), i = 1,5)]
  print *, ' 1:', iarr
  iarr = [item:: iarr]
  print *, ' 2:', iarr
  iarr = [item:: iarr(5:1:-1)]
  print *, ' 3:', iarr
  inx  = [integer:: 5, 1, 4, 3, 2]
  print *, ' 4:', [item:: (iarr(inx(i)), i=1,5)]
  iarr = [item:: (iarr(inx(i)), i=1,5)]
  print *, ' 5:', iarr
  iarr(1:3) = [item:: iarr(3:5)]
  print *, ' 6:', iarr
  iarr = [item:: item(1), item(2), item(3), item(4), item(5)]
  iarr(1:5:2) = [item:: iarr(5:3:-1)]
  print *, ' 7:', iarr
  iarr = [item:: item(1), item(2), item(3), item(4), item(5)]
  iarr(5:1:-2) = [item:: iarr(5:3:-1)]                    ! iarr(5,3,1) = 5,4,3 => 3 2 4 4 5
  print *, ' 8:', iarr
  iarr = [item:: item(1), item(2), item(3), item(4), item(5)]
  iarr(iarr(5:1:-2)%val) = [item:: iarr([integer:: 4, 1, 2])] ! iarr(5,3,1) = 4,1,2 => 2 2 1 4 4
  print *, ' 9:', iarr
  iarr = [item:: item(1), item(2), item(3), item(4), item(5)]
  iarr([integer:: (iarr(i:i-1:-1)%val, i=2,5,3)]) = [item:: iarr(5:2:-1)] ! iarr(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '10:', iarr

  ! implicit allocation the first time, thereafter it should reuse space
  iarra = [item:: (item(i ** 2), i = 1,5)]
  print *, '11:', iarra
  iarra = [item:: iarra]
  print *, '12:', iarra
  iarra = [item:: iarra(5:1:-1)]
  print *, '13:', iarra
  inx  = [integer:: 5, 1, 4, 3, 2]
  print *, '14:', [item:: (iarra(inx(i)), i=1,5)]
  iarra = [item:: (iarra(inx(i)), i=1,5)]
  print *, '15:', iarra
  iarra(1:3) = [item:: iarra(3:5)]
  print *, '16:', iarra
  iarra = [item:: item(1), item(2), item(3), item(4), item(5)]
  iarra(1:5:2) = [item:: iarra(5:3:-1)]
  print *, '17:', iarra
  iarra = [item:: item(1), item(2), item(3), item(4), item(5)]
  iarra(5:1:-2) = [item:: iarra(5:3:-1)]
  print *, '18:', iarra
  iarra = [item:: item(1), item(2), item(3), item(4), item(5)]
  iarra(iarra(5:1:-2)%val) = [item:: iarra([integer:: 4, 1, 2])] ! iarra(5,3,1) = 4,1,2 => 2 2 1 4 4
  print *, '19:', iarra
  iarra = [item:: item(1), item(2), item(3), item(4), item(5)]
  iarra([integer:: (iarra(i:i-1:-1)%val, i=2,5,3)]) = [item:: iarra(5:2:-1)] ! iarra(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '20:', iarra

  ! Repeat, but changing the size each time, to force reallocation:
  iarra = [item:: (item(i ** 2), i = 1,6)]
  print *, '21:', iarra
  iarra = [item:: iarra(5:2:-1), iarra(2:5)]
  print *, '22:', iarra
  inx  = [integer:: 7, 4, 1, 3, -1] ! only use the first four
  print *, '23:', [item:: (iarra(inx(i)), i=1,4), item(1)]
  iarra = [item:: (iarra(inx(i)), i=1,4), item(1)]
  print *, '24:', iarra
  iarra(1:3) = [item:: iarra(3:5)]
  print *, '25:', iarra
  iarra = [item:: item(1), item(2), item(3), item(4), item(5), item(6)]
  iarra(1:5:2) = [item:: iarra(5:3:-1)]
  print *, '26:', iarra
  iarra = [item:: item(1), item(2), item(3), item(4), item(5)]
  iarra(5:1:-2) = [item:: iarra(5:3:-1)]                     ! iarra(5,3,1) = 5,4,3 => 3 2 4 4 5
  print *, '27:', iarra
  iarra = [item:: item(1), item(2), item(3), item(4), item(5), item(6)]
  iarra(iarra(5:1:-2)%val) = [item:: iarra([integer:: 4, 1, 2])] ! iarra(5,3,1) = 4,1,2 => 2 2 1 4 4 6
  print *, '28:', iarra
  iarra = [item:: item(1), item(2), item(3), item(4), item(5)]
  iarra([integer:: (iarra(i:i-1:-1)%val, i=2,5,3)]) = [item:: iarra(5:2:-1)] ! iarra(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '29:', iarra

  ! try a sampling of tests accessing a field in a derived type object:
  dt%ivals = [item:: (item(i ** 2), i = 1,5)]
  print *, '30:', dt%ivals
  dt%ivals = [item:: dt%ivals]
  print *, '31:', dt%ivals
  dt%ivals = [item:: dt%ivals(5:1:-1)]
  print *, '32:', dt%ivals
  inx  = [integer:: 5, 1, 4, 3, 2]
  print *, '33:', [item:: (dt%ivals(inx(i)), i=1,5)]
  dt%ivals = [item:: (dt%ivals(inx(i)), i=1,5)]
  print *, '34:', dt%ivals
  dt%ivals(1:3) = [item:: dt%ivals(3:5)]
  print *, '35:', dt%ivals
  dt%ivals = [item:: item(1), item(2), item(3), item(4), item(5)]
  dt%ivals(1:5:2) = [item:: dt%ivals(5:3:-1)]
  print *, '36:', dt%ivals
  dt%ivals = [item:: item(1), item(2), item(3), item(4), item(5)]
  dt%ivals(5:1:-2) = [item:: dt%ivals(5:3:-1)]                    ! dt%ivals(5,3,1) = 5,4,3 => 3 2 4 4 5
  print *, '37:', dt%ivals
  dt%ivals = [item:: item(1), item(2), item(3), item(4), item(5)]
  dt%ivals(dt%ivals(5:1:-2)%val) = [item:: dt%ivals([integer:: 4, 1, 2])] ! dt%ivals(5,3,1) = 4,1,2 => 2 2 1 4 4
  print *, '38:', dt%ivals
  dt%ivals = [item:: item(1), item(2), item(3), item(4), item(5)]
  dt%ivals([integer:: (dt%ivals(i:i-1:-1)%val, i=2,5,3)]) = [item:: dt%ivals(5:2:-1)] ! dt%ivals(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '39:', dt%ivals

  ! and repeat for an allocatable component:
  dt%iavals = [item:: (item(i ** 2), i = 1,6)]
  print *, '40:', dt%iavals
  dt%iavals = [item:: dt%iavals]
  print *, '41:', dt%iavals
  dt%iavals = [item:: dt%iavals(5:2:-1), dt%iavals(2:5)]
  print *, '42:', dt%iavals
  inx  = [integer:: 7, 4, 1, 3, -1] ! only use the first four
  print *, '43:', [item:: (dt%iavals(inx(i)), i=1,4), item(1)]
  dt%iavals = [item:: (dt%iavals(inx(i)), i=1,4), item(1)]
  print *, '44:', dt%iavals
  dt%iavals(1:3) = [item:: dt%iavals(3:5)]
  print *, '45:', dt%iavals
  dt%iavals = [item:: item(1), item(2), item(3), item(4), item(5), item(6)]
  dt%iavals(1:5:2) = [item:: dt%iavals(5:3:-1)]
  print *, '46:', dt%iavals
  dt%iavals = [item:: item(1), item(2), item(3), item(4), item(5)]
  dt%iavals(5:1:-2) = [item:: dt%iavals(5:3:-1)]                     ! dt%iavals(5,3,1) = 5,4,3 => 3 2 4 4 5
  print *, '47:', dt%iavals
  dt%iavals = [item:: item(1), item(2), item(3), item(4), item(5), item(6)]
  dt%iavals(dt%iavals(5:1:-2)%val) = [item:: dt%iavals([integer:: 4, 1, 2])] ! dt%iavals(5,3,1) = 4,1,2 => 2 2 1 4 4 6
  print *, '48:', dt%iavals
  dt%iavals = [item:: item(1), item(2), item(3), item(4), item(5)]
  dt%iavals([integer:: (dt%iavals(i:i-1:-1)%val, i=2,5,3)]) = [item:: dt%iavals(5:2:-1)] ! dt%iavals(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '49:', dt%iavals

end program acetdt59
