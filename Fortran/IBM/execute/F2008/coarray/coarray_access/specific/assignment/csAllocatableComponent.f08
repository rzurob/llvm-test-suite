!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-10-04
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - assignment
!*  SECONDARY FUNCTIONS TESTED : in main program, assign allocatable intrinsic components of derived type variables to coarray variables and vice-versa
!*  ADAPTED FROM               : csAllocatableDT (<-csAllocatableIntrinsic<-csSimpleInteger<-csSimpleLogical)
!*
!*  DESCRIPTION
!*
!*  Assign simple values from allocatable intrinsic components of derived type
!*  variables to coarray scalars and arrays of different kinds in the main program.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csAllocatableComponent

    use ieee_arithmetic
    implicit none

    type base
       integer(2), allocatable :: f1
    end type base

    type, extends (base) :: der(l)
       integer, len :: l
       character(l), allocatable :: f2
    end type der

    type, extends (der) :: der2(k)
       integer, kind :: k
       real(k), allocatable :: f3
    end type der2

    type(der2(3,4)) :: v
    type(der2(3,4)) :: a(10)

    integer(2), parameter  :: par2 = -huge(0_2)-1
    character(3), parameter :: par3 = '#$%'
    integer, parameter :: A_CAP = iachar('A')
    real(4), parameter     :: par4 = -huge(0_4)-1

    integer(2), save :: i2[*] = 0, ia2(10)[*] = 0
    character(3), save :: c3[*] = '', ca3(10)[*] = ''
    real(4), save :: r4[*] = 0, ra4(10)[*] = 0

    integer :: i

    ! start with the minimum value for each kind:
    v = der2(3,4)(par2,par3,par4)
    a = [(der2(3,4)(i,repeat(achar(A_CAP+i-1),3),1.0/i), i=1,10)]

    i2 = v%f1
    c3 = v%f2
    r4 = v%f3

    do i = 1, 10
        ia2(i) = a(i)%f1
        ca3(i) = a(i)%f2
        ra4(i) = a(i)%f3
    end do

    if (i2/=par2 .or. c3/=par3 .or. .not.same4(r4,par4)) error stop 2
    if (any(ia2 /= [(i,i=1,10)])) error stop 3
    if (any(ca3 /= [(repeat(achar(A_CAP+i-1),3),i=1,10)])) error stop 4
    if (any(.not.same4(ra4,[(1.0/i,i=1,10)]))) error stop 5

    ! now test assignment *from* coarrays (first, reset targets)
    v = der2(3,4)(0,'',0.0)
    a = v

    v%f1 = i2
    v%f2 = c3
    v%f3 = r4

    do i = 1, 10
      a(i)%f1 = ia2(i)
      a(i)%f2 = ca3(i)
      a(i)%f3 = ra4(i)
    end do

    if (v%f1/=par2 .or. v%f2/=par3 .or. .not.same4(v%f3,par4)) error stop 12
    if (any([(a(i)%f1,i=1,10)] /= [(i,i=1,10)])) error stop 13
    if (any([(a(i)%f2,i=1,10)] /= [(repeat(achar(A_CAP+i-1),3),i=1,10)])) error stop 14
    if (any(.not.same4([(a(i)%f3,i=1,10)],[(1.0/i,i=1,10)]))) error stop 15

contains

  elemental logical function same4(a1,a2)
    real(4), intent(in) :: a1, a2
    real(4) :: r1, r2
    same4 = .true.
    r1 = a1
    r2 = a2
    ! covers exact equality, Inf and NaN:
    if (r1 == r2 .or. ieee_is_nan(r1) .and. ieee_is_nan(r1)) return
    if (.not.ieee_is_normal(r1) .or. .not.ieee_is_normal(r2)) then
      r1 = 1e20 * r1
      r2 = 1e20 * r2
    end if
    ! covers approximate equality:
    same4 = abs(r1 - r2) <= abs((r1*0.5E-5 + r2*0.5E-5)) ! avoiding overflow on max
  end function same4

end program csAllocatableComponent
