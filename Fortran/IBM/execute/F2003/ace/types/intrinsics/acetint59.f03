!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-11-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : self-referential assignment, including array sections (integers)
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
!*  Create AC's which refer to the variable to which they will be assigned;
!*  include array sections, especially on the LHS of assignments.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetint59mod

  implicit none
  type derived
     integer :: ivals(5)
     integer, allocatable :: iavals(:)
  end type derived

end module acetint59mod


program acetint59

  use acetint59mod
  implicit none
  integer :: iarr(5), inx(5), i
  integer, allocatable :: iarra(:)
  type (derived) :: dt

  iarr = [integer:: (i ** 2, i = 1,5)]
  print *, ' 1:', iarr
  iarr = [integer:: iarr]
  print *, ' 2:', iarr
  iarr = [integer:: iarr(5:1:-1)]
  print *, ' 3:', iarr
  inx  = [integer:: 5, 1, 4, 3, 2]
  print *, ' 4:', [integer:: (iarr(inx(i)), i=1,5)]
  iarr = [integer:: (iarr(inx(i)), i=1,5)]
  print *, ' 5:', iarr
  iarr(1:3) = [integer:: iarr(3:5)]
  print *, ' 6:', iarr
  iarr = [integer:: 1, 2, 3, 4, 5]
  iarr(1:5:2) = [integer:: iarr(5:3:-1)]
  print *, ' 7:', iarr
  iarr = [integer:: 1, 2, 3, 4, 5]
  iarr(5:1:-2) = [integer:: iarr(5:3:-1)]                    ! iarr(5,3,1) = 5,4,3 => 3 2 4 4 5
  print *, ' 8:', iarr
  iarr = [integer:: 1, 2, 3, 4, 5]
  iarr(iarr(5:1:-2)) = [integer:: iarr([integer:: 4, 1, 2])] ! iarr(5,3,1) = 4,1,2 => 2 2 1 4 4
  print *, ' 9:', iarr
  iarr = [integer:: 1, 2, 3, 4, 5]
  iarr([integer:: (iarr(i:i-1:-1), i=2,5,3)]) = [integer:: iarr(5:2:-1)] ! iarr(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '10:', iarr

  ! implicit allocation the first time, thereafter it should reuse space
  iarra = [integer:: (i ** 2, i = 1,5)]
  print *, '11:', iarra
  iarra = [integer:: iarra]
  print *, '12:', iarra
  iarra = [integer:: iarra(5:1:-1)]
  print *, '13:', iarra
  inx  = [integer:: 5, 1, 4, 3, 2]
  print *, '14:', [integer:: (iarra(inx(i)), i=1,5)]
  iarra = [integer:: (iarra(inx(i)), i=1,5)]
  print *, '15:', iarra
  iarra(1:3) = [integer:: iarra(3:5)]
  print *, '16:', iarra
  iarra = [integer:: 1, 2, 3, 4, 5]
  iarra(1:5:2) = [integer:: iarra(5:3:-1)]
  print *, '17:', iarra
  iarra = [integer:: 1, 2, 3, 4, 5]
  iarra(5:1:-2) = [integer:: iarra(5:3:-1)]
  print *, '18:', iarra
  iarra = [integer:: 1, 2, 3, 4, 5]
  iarra(iarra(5:1:-2)) = [integer:: iarra([integer:: 4, 1, 2])] ! iarra(5,3,1) = 4,1,2 => 2 2 1 4 4
  print *, '19:', iarra
  iarra = [integer:: 1, 2, 3, 4, 5]
  iarra([integer:: (iarra(i:i-1:-1), i=2,5,3)]) = [integer:: iarra(5:2:-1)] ! iarra(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '20:', iarra

  ! Repeat, but changing the size each time, to force reallocation:
  iarra = [integer:: (i ** 2, i = 1,6)]
  print *, '21:', iarra
  iarra = [integer:: iarra(5:2:-1), iarra(2:5)]
  print *, '22:', iarra
  inx  = [integer:: 7, 4, 1, 3, -1] ! only use the first four
  print *, '23:', [integer:: (iarra(inx(i)), i=1,4), 1]
  iarra = [integer:: (iarra(inx(i)), i=1,4), 1]
  print *, '24:', iarra
  iarra(1:3) = [integer:: iarra(3:5)]
  print *, '25:', iarra
  iarra = [integer:: 1, 2, 3, 4, 5, 6]
  iarra(1:5:2) = [integer:: iarra(5:3:-1)]
  print *, '26:', iarra
  iarra = [integer:: 1, 2, 3, 4, 5]
  iarra(5:1:-2) = [integer:: iarra(5:3:-1)]                     ! iarra(5,3,1) = 5,4,3 => 3 2 4 4 5
  print *, '27:', iarra
  iarra = [integer:: 1, 2, 3, 4, 5, 6]
  iarra(iarra(5:1:-2)) = [integer:: iarra([integer:: 4, 1, 2])] ! iarra(5,3,1) = 4,1,2 => 2 2 1 4 4 6
  print *, '28:', iarra
  iarra = [integer:: 1, 2, 3, 4, 5]
  iarra([integer:: (iarra(i:i-1:-1), i=2,5,3)]) = [integer:: iarra(5:2:-1)] ! iarra(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '29:', iarra

  ! try a sampling of tests accessing a field in a derived type object:
  dt%ivals = [integer:: (i ** 2, i = 1,5)]
  print *, '30:', dt%ivals
  dt%ivals = [integer:: dt%ivals]
  print *, '31:', dt%ivals
  dt%ivals = [integer:: dt%ivals(5:1:-1)]
  print *, '32:', dt%ivals
  inx  = [integer:: 5, 1, 4, 3, 2]
  print *, '33:', [integer:: (dt%ivals(inx(i)), i=1,5)]
  dt%ivals = [integer:: (dt%ivals(inx(i)), i=1,5)]
  print *, '34:', dt%ivals
  dt%ivals(1:3) = [integer:: dt%ivals(3:5)]
  print *, '35:', dt%ivals
  dt%ivals = [integer:: 1, 2, 3, 4, 5]
  dt%ivals(1:5:2) = [integer:: dt%ivals(5:3:-1)]
  print *, '36:', dt%ivals
  dt%ivals = [integer:: 1, 2, 3, 4, 5]
  dt%ivals(5:1:-2) = [integer:: dt%ivals(5:3:-1)]                    ! dt%ivals(5,3,1) = 5,4,3 => 3 2 4 4 5
  print *, '37:', dt%ivals
  dt%ivals = [integer:: 1, 2, 3, 4, 5]
  dt%ivals(dt%ivals(5:1:-2)) = [integer:: dt%ivals([integer:: 4, 1, 2])] ! dt%ivals(5,3,1) = 4,1,2 => 2 2 1 4 4
  print *, '38:', dt%ivals
  dt%ivals = [integer:: 1, 2, 3, 4, 5]
  dt%ivals([integer:: (dt%ivals(i:i-1:-1), i=2,5,3)]) = [integer:: dt%ivals(5:2:-1)] ! dt%ivals(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '39:', dt%ivals

  ! and repeat for an allocatable component:
  dt%iavals = [integer:: (i ** 2, i = 1,6)]
  print *, '40:', dt%iavals
  dt%iavals = [integer:: dt%iavals]
  print *, '41:', dt%iavals
  dt%iavals = [integer:: dt%iavals(5:2:-1), dt%iavals(2:5)]
  print *, '42:', dt%iavals
  inx  = [integer:: 7, 4, 1, 3, -1] ! only use the first four
  print *, '43:', [integer:: (dt%iavals(inx(i)), i=1,4), 1]
  dt%iavals = [integer:: (dt%iavals(inx(i)), i=1,4), 1]
  print *, '44:', dt%iavals
  dt%iavals(1:3) = [integer:: dt%iavals(3:5)]
  print *, '45:', dt%iavals
  dt%iavals = [integer:: 1, 2, 3, 4, 5, 6]
  dt%iavals(1:5:2) = [integer:: dt%iavals(5:3:-1)]
  print *, '46:', dt%iavals
  dt%iavals = [integer:: 1, 2, 3, 4, 5]
  dt%iavals(5:1:-2) = [integer:: dt%iavals(5:3:-1)]                     ! dt%iavals(5,3,1) = 5,4,3 => 3 2 4 4 5
  print *, '47:', dt%iavals
  dt%iavals = [integer:: 1, 2, 3, 4, 5, 6]
  dt%iavals(dt%iavals(5:1:-2)) = [integer:: dt%iavals([integer:: 4, 1, 2])] ! dt%iavals(5,3,1) = 4,1,2 => 2 2 1 4 4 6
  print *, '48:', dt%iavals
  dt%iavals = [integer:: 1, 2, 3, 4, 5]
  dt%iavals([integer:: (dt%iavals(i:i-1:-1), i=2,5,3)]) = [integer:: dt%iavals(5:2:-1)] ! dt%iavals(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '49:', dt%iavals

end program acetint59
