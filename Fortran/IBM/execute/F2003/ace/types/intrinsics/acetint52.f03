!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-11-17
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : ac-do-variable name scope
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ac-do-variable, scope
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Create AC-implied do's with correct nesting, using I/O, function call and
!*  assignment statements.  Verify that any same-named variables outside the
!*  scope of the statements with the implied-do's are unaffected.  By "correct
!*  nesting", we mean that control variables are unique in a particular scope
!*  within a statement (e.g., "((...,i=1,1),...i=2,2)" is not legal), and do
!*  not interact with non-control variables by the same name (although they
!*  have the same type).  Therefore, we need to verify that AC's do not alter
!*  such non-control variables and are not altered by the use of same within
!*  the AC (e.g., if altered by a function invoked from within the AC).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint52

  implicit none

  integer :: i, j, k, arr(14), iOld, jOld

  i = -1
  j = -2
  k = -3

  do i=6,8,2
     do j=10,15,5
        iOld = i
        jOld = j
        print *, [integer:: i, j, ((j, i, j=1,2), (j,i,j=3,3), i=1,2)], [character:: (char(i), i=65,90)]
        if (i /= iOld .or. j /= jOld) error stop 2
        arr = [integer:: i, j, ((j, i, j=1,2), (j,i,j=3,3), i=1,2)]
        if (i /= iOld .or. j /= jOld) error stop 3
        print *, arr
        call sub(i,j, [integer:: i, j, ((j, i, j=1,2), (j,i,j=3,3), i=1,2)])
        if (i /= iOld .or. j /= jOld) error stop 4
        k = func(i,j, [integer:: i, j, ((j, i, j=1,2), (j,i,j=3,3), i=1,2)])
        print *, k
        if (i /= iOld .or. j /= jOld) error stop 5
     end do
  end do

  i = 1
  j = 2
  k = 3

  print *, [integer:: ((i,j,func2([(j,i,k=1,2)]), j=1,3),i=1,2)]
  arr(1:12) = [integer:: ((i,j,func2([(j,i,k=1,2)]), j=1,2),i=3,4)]
  print *, arr(1:12)
  call sub(i,j, [integer:: ((i,j,func2([(j,i,k=1,2)]), j=1,3),i=1,2)])
  k = func(i,j, [integer:: ((i,j,func2([(j,i,k=1,2)]), j=1,3),i=1,2)])
  print *, k

contains

  subroutine sub(arg1, arg2, arg3)
    integer :: arg1, arg2, arg3(:)
    print *, 'sub:', arg1, arg2, arg3, [((i*j,i=3,4),j=5,6)]
  end subroutine sub

  integer function func(arg1, arg2, arg3)
    integer :: arg1, arg2, arg3(:)
    print *, 'func:', arg1, arg2, arg3, ',i:', i, ',j:', j, [((i*j,i=3,4),j=5,6)]
    func = sum([((arg3(i+j),i=1,2),j=1,2)]) + arg2 ! 2,3,3,4
  end function func

  integer function func2(arg1)
    integer :: arg1(3:4,1:*)
    func2 = 1
    do i = 1,2
       do j = 3,4
          func2 = func2 * arg1(j,i)
       end do
    end do
    i = -1
    j = -2
  end function func2

end program acetint52
