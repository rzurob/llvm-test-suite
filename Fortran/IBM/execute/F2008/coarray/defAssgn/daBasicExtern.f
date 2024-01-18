!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : daBasicExtern
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2010-12-20
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 CAF coarray defined assignment
!*  SECONDARY FUNCTIONS TESTED : 'basic' externally defined subroutine ???
!*
!*  DESCRIPTION
!*
!*  ---
!*
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program daBasicExtern
  interface assignment ( = )
     subroutine intAssign(x, y)
       integer, intent(inout) :: x(:)[*]
       integer, intent(in) :: y(:,:)[*]
     end subroutine intAssign
  end interface assignment ( = )
  integer, save :: c1(3)[*], c2(3,4)[*]
  integer :: v1, v2, i, me

  ! intrinsic assignment to start:
  me = this_image()
  c1 = [(10000*i+me*1000, i=1,3)]
  c2 = reshape([((j*100+i*10+me,j=1,3),i=1,4)],[3,4])
  if (me == 1) print *, c1, c2
  ! now do the defined assignment:
  c1 = c2
  if (me == 1) print *, c1, c2
  if (any(c2 /= reshape([((j*100+i*10+me,j=1,3),i=1,4)],[3,4]))) error stop 2
  if (any(c1 /= [(10000*i+me*1000 + sum(c2(i,:)), i=1,3)])) error stop 3
end program daBasicExtern

subroutine intAssign(x, y)
  integer, intent(inout) :: x(:)[*]
  integer, intent(in) :: y(:,:)[*]
  integer :: i
  do i = 1, size(x)
     x(i) = x(i) + sum(y(i,:))
  end do
end subroutine intAssign
