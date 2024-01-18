!*  ===================================================================
!*
!*  DATE                       : July 31, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : THIS_IMAGE intrinsic
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : functional testing of the THIS_IMAGE
!*                               intrinsic of CAF. This test case was
!*                               originally from Rice Univ. Changes
!*                               have been made to enhance it.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program intrinsic_procedures
  call this_image_test()
  sync all
end program intrinsic_procedures

subroutine this_image_test
  integer, save:: a[*], b(10)[2,*], c(10,10)[3,2,*]

  integer me, meA(1), meB1, meB2, meC1, meC2, meC3
  integer i,j
  integer thisImageBVector(2)
  integer thisImageCVector(3)
  integer dd(3)

  interface
     function foo1(a)
       integer a(1), foo1(1)
     end function foo1
     function foo2(a)
       integer a(2), foo2(2)
     end function foo2
     function foo3(a)
       integer a(3), foo3(3)
     end function foo3
  end interface

  ! case i
  ! Invoke THIS_IMAGE() without any argument.  This returns
  ! the image index of the invoking image
  me = this_image()
  print *,me,': me=',me

  ! case ii
  ! Invoke THIS_IMAGE() with the COARRAY argument.  This returns the
  ! sequence of cosubscript values for COARRAY that would specify the
  ! invoking image.
  meA = this_image(a)
  print *,me,': this_image(a)=',meA
  thisImageBVector = this_image(B)
  print *,me,': this_image(b)=',this_image(B)
  thisImageCVector = this_image(C)
  print *,me,': this_image(c)=',thisImageCVector

  ! case iii
  ! The same as case ii, except passing THIS_IMAGE to a function
  print *,me,': this_image(a)=', foo1(this_image(a))
  print *,me,': this_image(b)=', foo2(this_image(b))
  print *,me,': this_image(c)=', foo3(this_image(c))

  ! case iv
  ! Invoke THIS_IMAGE() with the COARRAY and DIM arguments. This returns
  ! the value of cosubscript DIM in the sequence of cosubscript values for
  ! COARRAY that would specify the invoking image.
  meA1 = this_image(a,1)
  print *, me,': meA1=',meA1

  meB1 = this_image(b,1)
  meB2 = this_image(b,2)
  print *, me,': meB1=',meB1, 'meB2=', meB2

  meC1 = this_image(c,1)
  meC2 = this_image(c,2)
  meC3 = this_image(c,3)
  print *, me,': meC1=',meC1,', meC2=',meC2,', meC3=',meC3

  ! case v
  ! Same as case iv, except variables are used instead of constants
  ! for argument DIM.
  i=1
  j=2

  meC1 = this_image(c,i)
  meC2 = this_image(c,j)
  meC3 = this_image(c,j+i)
  print *, me,': meC1=',meC1,', meC2=',meC2,', meC3=',meC3

end subroutine this_image_test

function foo1(a)
  integer a(1), foo1(1)
  foo1 = a
end function foo1

function foo2(a)
  integer a(2), foo2(2)
  foo2 = a
end function foo2

function foo3(a)
  integer a(3), foo3(3)
  foo3 = a
end function foo3
