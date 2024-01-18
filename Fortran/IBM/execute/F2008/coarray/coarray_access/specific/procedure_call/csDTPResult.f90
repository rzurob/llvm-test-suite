!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : csDTPResult
!*
!*  DATE                       : 2010-10-15
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - procedure call
!*  SECONDARY FUNCTIONS TESTED : function accepts coarray actual argument and returns derived type object (i.e., constructs the derived type object from data stored in coarray variables)
!*  ADAPTED FROM               : csAllocatableResult (<-csAllocatableDT)
!*
!*  DESCRIPTION
!*
!*  Copy coarray arguments to a structure and return it as a function result.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csDTPResult

    use ieee_arithmetic
    implicit none

    type base
       integer(2) :: f1
    end type base

    type, extends (base) :: der(l)
       integer, len :: l
       character(l) :: f2
    end type der

    type, extends (der) :: der2(k)
       integer, kind :: k
       real(k) :: f3
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

    i2 = par2
    c3 = par3
    r4 = par4
    v = copy(i2, c3, r4)

    ia2 = [(i, i=1,10)]
    ca3 = [(repeat(achar(A_CAP+i-1),3), i=1,10)]
    ra4 = [(1.0/i, i=1,10)]
    a = acopy(ia2, ca3, ra4)

    if (v%f1/=par2 .or. v%f2/=par3 .or. .not.same4(v%f3,par4)) error stop 2
    if (any(a(:)%f1 /= [(i,i=1,10)])) error stop 3
    if (any(a(:)%f2 /= [(repeat(achar(A_CAP+i-1),3),i=1,10)])) error stop 4
    if (any(.not.same4(a(:)%f3,[(1.0/i,i=1,10)]))) error stop 5

contains

  type(der2(3,4)) function copy(v2, v3, v4)
    integer(2) :: v2[*]
    character(3) :: v3[*]
    real(4) :: v4[*]
    copy = der2(3,4)(v2,v3,v4)
  end function copy

  function acopy(v2, v3, v4)
    integer(2) :: v2(:)[*]
    character(3) :: v3(:)[*]
    real(4) :: v4(:)[*]
    type(der2(3,4)) :: acopy(size(v2))
    integer :: i

    do i = 1, size(v2)
      acopy(i) = der2(3,4)(v2(i),v3(i),v4(i))
    end do
  end function acopy

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

end program csDTPResult
