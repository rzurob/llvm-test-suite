!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : daAll_DT
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2010-12-20
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 CAF coarray defined assignment
!*  SECONDARY FUNCTIONS TESTED : LHS is any non-char coarray intrinsic type, RHS is noncoarray derived type
!* keywords:	dt	mod	non-conf	out	dnn	elem	mod proc	var
!*
!*  DESCRIPTION
!*
!*  LHS is any non-char coarray intrinsic type and RHS is noncoarray derived type.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtmod
  type :: dt
     integer(4)   :: icomp(3)
     complex(4)   :: zcomp
     real(8)      :: rcomp
     logical(1)   :: lcomp
     type(dt), pointer :: next => null()
  end type
end module


program dAll_DT
  use :: dtmod
  type(dt), target :: d1
  type(dt)     :: d2
  integer(4), save   :: iv(3)[*]
  complex(4), save   :: zv[*]
  real(8), save      :: rv[*]
  logical(1), save   :: lv[*]

  interface assignment(=)
     subroutine i_d_assign(lhs,rhs)
       import :: dt
       integer(4), intent(out) :: lhs(3)
       type(dt), intent(in) :: rhs
     end subroutine
     subroutine z_d_assign(lhs,rhs)
       import :: dt
       complex(4), intent(out) :: lhs
       type(dt), intent(in) :: rhs
     end subroutine
     subroutine r_d_assign(lhs,rhs)
       import :: dt
       real(8), intent(out) :: lhs
       type(dt), intent(in) :: rhs
     end subroutine
     subroutine l_d_assign(lhs,rhs)
       import :: dt
       logical(1), intent(out) :: lhs
       type(dt), intent(in) :: rhs
     end subroutine
  end interface

  d1 = dt([1,2,3],(1.1,2.2),9.876543d2,.true.)
  d2 = dt([99,98,97],(-.1,-.2),3.1415269d0,.false.,d1)
  iv = d2
  zv = d2
  rv = d2
  lv = d2
  
  if ( any(iv .ne. [99,98,97]) ) then
	print *, iv
	error stop 11
  end if
  if (zv /= (-0.1,-0.2)) then
	print *, zv
	error stop 12
  end if
  if (rv /= 3.1415269d0) then
	print *, rv
	error stop 13
  end if
  if (lv) then
	print *, lv
	error stop 14
  end if
  
  
  iv = d1
  zv = d1
  rv = d1
  lv = d1
  
  if ( any(iv .ne. [1,2,3]) ) then
	print *, iv
	error stop 15
  end if
  if (zv /= (1.1,2.2)) then
	print *, zv
	error stop 16
  end if
  if (rv /= 9.876543d2) then
	print *, rv
	error stop 17
  end if
  if (.not. lv) then
	print *, lv
	error stop 18
  end if
end program dAll_DT


subroutine i_d_assign(lhs,rhs)
  use :: dtmod
  integer(4), intent(out) :: lhs(3)
  type(dt), intent(in) :: rhs
  
  lhs = rhs%icomp
end subroutine

subroutine z_d_assign(lhs,rhs)
  use :: dtmod
  complex(4), intent(out) :: lhs
  type(dt), intent(in) :: rhs
  
  lhs = rhs%zcomp
end subroutine

subroutine r_d_assign(lhs,rhs)
  use :: dtmod
  real(8), intent(out) :: lhs
  type(dt), intent(in) :: rhs
  
  lhs = rhs%rcomp
end subroutine

subroutine l_d_assign(lhs,rhs)
  use :: dtmod
  logical(1), intent(out) :: lhs
  type(dt), intent(in) :: rhs
  
  lhs = rhs%lcomp
end subroutine
