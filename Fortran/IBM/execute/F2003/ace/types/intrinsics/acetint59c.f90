!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint59c
!*
!*  DATE                       : 2006-11-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : self-referential assignment, including array sections (characters)
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

module acetint59cmod

  implicit none
  type derived
     character :: vals(5)
     character(:), allocatable :: avals(:)
  end type derived

end module acetint59cmod


program acetint59c

  use acetint59cmod
  implicit none
  integer   :: inx(5), i
  character(3) :: arr(5)
  character(:), allocatable :: arra(:)
  type (derived) :: dt

  arr = [character(3):: (repeat(char(32 + i ** 2),3), i = 1,5)]
  print *, ' 1:', arr
  arr = [character(3):: arr]
  print *, ' 2:', arr
  arr = [character(3):: arr(5:1:-1)]
  print *, ' 3:', arr
  inx  = [integer:: 5, 1, 4, 3, 2]
  print *, ' 4:', [character(3):: (arr(inx(i)), i=1,5)]
  arr = [character(3):: (arr(inx(i)), i=1,5)]
  print *, ' 5:', arr
  arr(1:3) = [character(3):: arr(3:5)]
  print *, ' 6:', arr
  arr = [character(3):: 'aa.', 'bb.', 'cc.', 'dd.', 'ee.']
  arr(1:5:2) = [character(3):: arr(5:3:-1)]
  print *, ' 7:', arr
  arr = [character(3):: 'aa.', 'bb.', 'cc.', 'dd.', 'ee.']
  arr(5:1:-2) = [character(3):: arr(5:3:-1)]                    ! arr(5,3,1) = 5,4,3 => 3 2 4 4 5
  print *, ' 8:', arr
  arr = [character(3):: 'aa.', 'bb.', 'cc.', 'dd.', 'ee.']
  arr(mod(iachar(arr(5:1:-2)(1:1)),5)+1) = [character(3):: arr([integer:: 4, 1, 2])] ! arr(5,3,1) = 4,1,2 => 2 2 1 4 4
  print *, ' 9:', arr
  arr = [character(3):: 'aa.', 'bb.', 'cc.', 'dd.', 'ee.']
  arr([integer:: (mod(iachar(arr(i:i-1:-1)(1:1)),5)+1, i=2,5,3)]) = [character(3):: arr(5:2:-1)] ! arr(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '10:', arr

  ! implicit allocation the first time, thereafter it should reuse space
  arra = [character(5):: (repeat(char(32 + i ** 2),5), i = 1,5)]
  print *, '11:', arra
  arra = [character(5):: arra]
  print *, '12:', arra
  arra = [character(5):: arra(5:1:-1)]
  print *, '13:', arra
  inx  = [integer:: 5, 1, 4, 3, 2]
  print *, '14:', [character(5):: (arra(inx(i)), i=1,5)]
  arra = [character(5):: (arra(inx(i)), i=1,5)]
  print *, '15:', arra
  arra(1:3) = [character(5):: arra(3:5)]
  print *, '16:', arra
  arra = [character(5):: '<AAA>', '<BBB>', '<CCC>', '<DDD>', '<EEE>']
  arra(1:5:2) = [character(5):: arra(5:3:-1)]
  print *, '17:', arra
  arra = [character(5):: '<AAA>', '<BBB>', '<CCC>', '<DDD>', '<EEE>']
  arra(5:1:-2) = [character(5):: arra(5:3:-1)]                    ! arra(5,3,1) = 5,4,3 => 3 2 4 4 5
  print *, '18:', arra
  arra = [character(5):: '<AAA>', '<BBB>', '<CCC>', '<DDD>', '<EEE>']
  arra(mod(iachar(arra(5:1:-2)(2:2)),5)+1) = [character(5):: arra([integer:: 4, 1, 2])] ! arra(5,3,1) = 4,1,2 => 2 2 1 4 4
  print *, '19:', arra
  arra = [character(5):: '<AAA>', '<BBB>', '<CCC>', '<DDD>', '<EEE>']
  arra([integer:: (mod(iachar(arra(i:i-1:-1)(2:2)),5)+1, i=2,5,3)]) = [character(5):: arra(5:2:-1)] ! arra(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '20:', arra

  ! Repeat, but changing the length and size each time, to force reallocation:
  arra = [character(4):: (repeat(char(32 + i ** 2),4), i = 1,6)]
  print *, '21:', arra
  arra = [character(4):: arra]
  print *, '22:', arra
  arra = [character(5):: arra]
  print *, '23:', arra
  arra = [character(3):: arra(5:2:-1), arra(2:5)]
  print *, '24:', arra
  inx  = [integer:: 7, 4, 1, 3, -1] ! only use the first four
  print *, '25:', [character(4):: (arra(inx(i)), i=1,4), 'xxxx']
  arra = [character(4):: (arra(inx(i)), i=1,4), 'xxxx']
  print *, '26:', arra
  arra(1:3) = [character(3):: arra(3:5)]
  print *, '27:', arra
  arra = [character(2):: 'ff', 'gg', 'hh', 'ii', 'jj']
  arra(1:5:2) = [character(2):: arra(5:3:-1)]
  print *, '28:', arra
  arra = [character(1):: 'k', 'l', 'm', 'n', 'o']
  arra(5:1:-2) = [character(1):: arra(5:3:-1)]                     ! arra(5,3,1) = 5,4,3 => 3 2 4 4 5
  print *, '29:', arra
  arra = [character(3):: 'aa.', 'bb.', 'cc.', 'dd.', 'ee.']
  arra(mod(iachar(arra(5:1:-2)(1:1)),size(arra))+1) = [character(3):: arra([integer:: 4, 1, 2])] ! arra(5,3,1) = 4,1,2 => 2 2 1 4 4 6
  print *, '30:', arra
  arra = [character(5):: '<AAA>', '<BBB>', '<CCC>', '<DDD>', '<EEE>']
  arra([integer:: (mod(iachar(arra(i:i-1:-1)(2:2)),size(arra))+1, i=2,5,3)]) = [character(5):: arra(5:2:-1)] ! arra(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '31:', arra

  ! repeat, accessing a field in a derived type object:
  dt%vals = [character(1):: (char(32 + i ** 2), i = 1,5)]
  print *, '32:', dt%vals
  dt%vals = [character(1):: dt%vals]
  print *, '33:', dt%vals
  dt%vals = [character(1):: dt%vals(5:1:-1)]
  print *, '34:', dt%vals
  inx  = [integer:: 5, 1, 4, 3, 2]
  print *, '35:', [character(1):: (dt%vals(inx(i)), i=1,5)]
  dt%vals = [character(1):: (dt%vals(inx(i)), i=1,5)]
  print *, '36:', dt%vals
  dt%vals(1:3) = [character(1):: dt%vals(3:5)]
  print *, '37:', dt%vals
  dt%vals = [character(1):: 'a', 'b', 'c', 'd', 'e']
  dt%vals(1:5:2) = [character(1):: dt%vals(5:3:-1)]
  print *, '38:', dt%vals
  dt%vals = [character(1):: 'a', 'b', 'c', 'd', 'e']
  dt%vals(5:1:-2) = [character(1):: dt%vals(5:3:-1)]                    ! dt%vals(5,3,1) = 5,4,3 => 3 2 4 4 5
  print *, '39:', dt%vals
  dt%vals = [character(1):: 'a', 'b', 'c', 'd', 'e']
  dt%vals(mod(iachar(dt%vals(5:1:-2)(1:1)),size(dt%vals))+1) = [character(1):: dt%vals([integer:: 4, 1, 2])] ! dt%vals(5,3,1) = 4,1,2 => 2 2 1 4 4
  print *, '40:', dt%vals
  dt%vals = [character(1):: 'a', 'b', 'c', 'd', 'e']
  dt%vals([integer:: (mod(iachar(dt%vals(i:i-1:-1)(1:1)),size(dt%vals))+1, i=2,5,3)]) = [character(1):: dt%vals(5:2:-1)] ! dt%vals(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '41:', dt%vals

  ! Repeat, but changing the length and size each time, to force reallocation:
  dt%avals = [character(4):: (repeat(char(32 + i ** 2),4), i = 1,6)]
  print *, '42:', dt%avals
  dt%avals = [character(4):: dt%avals]
  print *, '43:', dt%avals
  dt%avals = [character(5):: dt%avals]
  print *, '44:', dt%avals
  dt%avals = [character(3):: dt%avals(5:2:-1), dt%avals(2:5)]
  print *, '45:', dt%avals
  inx  = [integer:: 7, 4, 1, 3, -1] ! only use the first four
  print *, '46:', [character(4):: (dt%avals(inx(i)), i=1,4), 'xxxx']
  dt%avals = [character(4):: (dt%avals(inx(i)), i=1,4), 'xxxx']
  print *, '47:', dt%avals
  dt%avals(1:3) = [character(3):: dt%avals(3:5)]
  print *, '48:', dt%avals
  dt%avals = [character(2):: 'ff', 'gg', 'hh', 'ii', 'jj']
  dt%avals(1:5:2) = [character(2):: dt%avals(5:3:-1)]
  print *, '49:', dt%avals
  dt%avals = [character(1):: 'k', 'l', 'm', 'n', 'o']
  dt%avals(5:1:-2) = [character(1):: dt%avals(5:3:-1)]                     ! dt%avals(5,3,1) = 5,4,3 => 3 2 4 4 5
  print *, '50:', dt%avals
  dt%avals = [character(3):: 'aa.', 'bb.', 'cc.', 'dd.', 'ee.']
  dt%avals(mod(iachar(dt%avals(5:1:-2)(1:1)),size(dt%avals))+1) = [character(3):: dt%avals([integer:: 4, 1, 2])] ! dt%avals(5,3,1) = 4,1,2 => 2 2 1 4 4 6
  print *, '51:', dt%avals
  dt%avals = [character(5):: '<AAA>', '<BBB>', '<CCC>', '<DDD>', '<EEE>']
  dt%avals([integer:: (mod(iachar(dt%avals(i:i-1:-1)(2:2)),size(dt%avals))+1, i=2,5,3)])(2:4) = [character(3):: dt%avals(5:2:-1)] ! dt%avals(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '52:', dt%avals

end program acetint59c
