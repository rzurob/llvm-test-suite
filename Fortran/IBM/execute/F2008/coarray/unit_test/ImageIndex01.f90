!*  ===================================================================
!*
!*  DATE                       : July 31, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : IMAGE_INDEX intrinsic
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : functional testing of the IMAGE_INDEX
!*                               intrinsic of CAF
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  program ImageIndex01
  integer, save:: a[*], b(10)[10,-2:*], c(10,10)[10,0:9,0:*]

  integer me
  integer ImageIndexAVector(1)
  integer ImageIndexBVector(2)
  integer ImageIndexCVector(3)
  integer ImageIndexA
  integer ImageIndexB
  integer ImageIndexC
  integer foo

  ! case i
  ! image_index() takes an array as the argument SUB
  me = this_image()
  ImageIndexAVector = [me]
  ImageIndexA = image_index(a, ImageIndexAVector)
  print *,me,': image_index(a, [me])=',ImageIndexA
  ImageIndexBVector = [me,-2]
  ImageIndexB = image_index(B, ImageIndexBVector)
  print *,me,': image_index(b, [me,-2])=',ImageIndexB
  ImageIndexCVector = [me,0,0]
  ImageIndexC = image_index(C, ImageIndexCVector)
  print *,me,': image_index(c, [me,0,0])=',ImageIndexC

  !case ii
  !  image_index() takes an array vector as the argument SUB
  ImageIndexA = image_index(a,[me])
  print *,me,': image_index(a, [me])=',ImageIndexA
  ImageIndexB = image_index(B, [me,-2])
  print *,me,': image_index(b, [me,-2])=',ImageIndexB
  ImageIndexC = image_index(C, [me,0,0])
  print *,me,': image_index(c, [me,0,0])=',ImageIndexC

  ! case iii
  ! image_index() is passsed as an argument to a function
  print *,me,': image_index(a, [me])=',foo(image_index(a, [me]))
  print *,me,': image_index(b, [me,-2])=',foo(image_index(b, [me,-2]))
  print *,me,': image_index(c, [me,0,0])=',foo(image_index(c, [me,0,0]))

  ! case iv
  ! image_index() returns 0 when SUB has invalid cosubscripts
  print *,me,': image_index(a, [0])=',foo(image_index(a, [0]))
  print *,me,': image_index(b, [me,-3])=',foo(image_index(b, [me,-3]))
  print *,me,': image_index(c, [me,10,0])=',foo(image_index(c, [me,10,0]))

sync all
end

function foo(a)
  integer foo, a
  foo = a
end function foo
