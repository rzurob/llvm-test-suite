! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qsmp=schedule=guided=5 -qfree=f90
! %GROUP: fxst011b.f
! %VERIFY: fxst011b.out:fxst011b.vf
! %STDIN:
! %STDOUT: fxst011b.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************

!*  ===================================================================
!*
!*  TEST CASE NAME             : fxst011b.f
!*
!*  DATE                       : October 10, 1996
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : -qsmp=schedule= compiler option
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  : (-qsmp)  -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Test Function Calls in do-loops
!*                               In this test, only pure functions
!*                               are called.
!*
!*                               Derived from fxst011a.f
!*                               Only compiler option is changed.
!234567890123456789012345678901234567890123456789012345678901234567890

program abstracti059
  implicit none
  integer :: j1,j2,j3,j4,j5,j6,j7,j8,j9


  abstract interface
     real pure function func1ai (i1)
       integer, intent(in) :: i1
     end function func1ai

     integer pure function func2ai (i1)
       integer, dimension(:), intent(in) :: i1
     end function func2ai

     pure function func3ai(i1) result (i2)
       integer, intent(in) :: i1
       integer, dimension(i1,i1), intent(out) :: i2
     end function func3ai

     pure function func4ai(i1) result (i2)
       integer, intent(in),  dimension(:,:) ::  i1
       integer, intent(out), dimension(ubound(i1,1) ) :: i2
     end function func4ai

  end interface

  procedure (func1ai) :: func1
  procedure (func2ai) :: func2
  procedure (func3ai) :: func3
  procedure (func4ai) :: func4

  ! Test 1
  real, dimension(10,10,10) :: r1

  ! Test 2
  integer, dimension(20 ) :: i2_1
  integer, dimension(4,17:20,1:4) :: i2_2

  ! Test 3
  integer, dimension(16,16) :: i3_2

  ! Test 4
  integer, dimension(4,4) :: i4_1
  integer, dimension(4,4,4,4) :: i4_2


  ! Test 1: Scalar argument, scalar results
  do j1 = 1,10
     do j2 = 1,10
        j3 = 0
        do
           j3= j3 +1
           if (j3 == 10 ) exit
           r1(j1,j2,j3) = func1(j1+j2+j3)

        end do
     end do
  end do

  print *, "Test 1"
  write(*, fmt="(6E12.3E3)") (((r1(j1,j2,j3), j1=1,10,4), j2=2,10,3), j3=3,9,4)


  ! Test 2: Array argument, scalar results

  do j1 = 1, 4
     do j2 = 17,20
        do j3 = 1,4
           i2_2(j1,j2,j3) = func2( i2_1(j1:j2:j3) )

        end do
     end do
  end do

  print *, "Test 2"
  write(*, fmt="(8i8)") (((i2_2(j1,j2,j3),j1=1,4,2), j2=17,20,2), j3=1,4)


  ! Test 3: Scalar argument, array results

  do j1= 16,1,-2
     i3_2(:j1,:j1) = func1(j1)
  end do

  print *, "Test 3"
  write(*, fmt="(8i8)") i3_2

  ! Test 4: Array Argument, array result

  i4_1 = reshape( (/(j1, j1=1,16)/), (/4,4/) )


  do j1 = 1,4
     do j2 = 1,4
         do j3 = 1, 4
            i4_2(j1,:,j2,j3) = func4( i4_1)
         end do
     end do
  end do

  print *, "Test 4"
  write(*, fmt="(8i8)") i4_2

end program abstracti059


real pure function func1 (i1)
  integer, intent(in) :: i1
  func1 = real(i1)
end function func1

integer pure function func2 (i1)
  integer, dimension(:), intent(in) :: i1
  func2=size(i1)
end function func2

pure function func3(i1) result (i2)
  integer, intent(in) :: i1
  integer, dimension(i1,i1), intent(out) :: i2
  i2 = i1
end function func3

pure function func4(i1) result (i2)
  integer, intent(in),  dimension(:,:) ::  i1
  integer, intent(out), dimension(ubound(i1,1) ) :: i2
  i2(:) = sum(i1,2)
end function func4
