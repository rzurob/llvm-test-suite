!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt00
!*
!*  DATE                       : 2006-09-27
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : derived TS, no parameters, simple objects of correct type
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : derived type
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Create arrays of different varieties of related derived types.
!*  Type 'derived' is a subclass of 'base'; type composed includes a component
!*  of type 'base'.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt00mod

  use, intrinsic :: ieee_arithmetic
  implicit none
  type :: base
     integer :: int
     real    :: rl
     complex :: z
   contains
     procedure :: check_equals => base_equals
     generic :: operator(.eq.) => check_equals
  end type base

  type, extends (base) :: derived
     character (1):: ch
   contains
     procedure :: check_equals => derived_equals
!     generic :: operator(.eq.) => derived_equals
  end type derived

  type :: composed
     type (base)  :: b1
     logical(1)   :: log
   contains
     procedure :: composed_equals
     generic :: operator(.eq.) => composed_equals
  end type composed

contains

  elemental logical function base_equals(a, b)
    class (base), intent(in) :: a, b
    base_equals = (a%int == b%int) .and. isSameReal4(a%rl,b%rl) .and. isSameComplex4(a%z,b%z)
  end function base_equals

  elemental logical function derived_equals(a, b)
    class (derived), intent(in) :: a
    class (base), intent(in) :: b

    select type (b)
        class is (derived)
            derived_equals = base_equals(a%base,b%base) .and. (a%ch == b%ch)

        class default
            derived_equals = .false.
    end select
  end function derived_equals

  elemental logical function composed_equals(a, b)
    class (composed), intent(in) :: a, b
    composed_equals = base_equals(a%b1,b%b1) .and. (a%log .eqv. b%log)
  end function composed_equals

  ! Adapted from our standard precision_R4 to be elemental, and to handle NaN and Inf:
  elemental logical function isSameReal4(value,expected)
    real(4), intent(in) :: value, expected
    real(4) :: high, low, delta

    ! If they're both extreme - both NaN or Infinite with the same sign - return true
    if (ieee_is_nan(value) .and. ieee_is_nan(expected) .or. (value == expected)) then
       isSameReal4 = .true.
    else
       delta = expected * 0.00001
       high = delta + expected
       low = expected - delta
       ! This is still not perfect: we don't handle the range near Inf well:
       if (expected < 0.0E0) then
          isSameReal4 = ((value >= high) .and. (value <= low))
       else
          isSameReal4 = ((value <= high) .and. (value >= low))
       end if
    end if

  end function isSameReal4

  elemental logical function isSameComplex4(value,expected)
    complex(4), intent(in) :: value, expected
    isSameComplex4 = isSameReal4(real(value),real(expected)) .and. isSameReal4(aimag(value),aimag(expected))
  end function isSameComplex4

end module acetdt00mod


program acetdt00

  use acetdt00mod
  implicit none

  type(base)     :: bas1(3), bas2(3), bas3(3), bas4(3), bas5(3)
  type(derived)  :: der1(3), der2(3), der3(3), der4(3), der5(3)
  type(composed) :: comp1(3), comp2(3), comp3(3), comp4(3), comp5(3)
  integer        :: i


  bas1 = [base(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0))), &
          base(1, 1e37, (huge(0.0),tiny(0.0))), &
          base(-1, huge(1e3), (1e-9,1e-37))]

  bas2 = [base:: base(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0))), &
                 base(1, 1e37, (huge(0.0),tiny(0.0))), &
                 base(-1, huge(1e3), (1e-9,1e-37))]

  bas3 = [base:: (base(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0))), &
                  base(1, 1e37, (huge(0.0),tiny(0.0))), &
                  base(-1, huge(1e3), (1e-9,1e-37)), i=2,2)]

  bas4 = [base:: bas3]

  if (.not.(all(bas1 == bas2) .and. all(bas2 == bas3) .and. all(bas3 == bas4) .and. all(bas4 == bas1))) stop 2

  bas5 = [base:: (base(i*100,1.0/i/3,(0.0-i,i+10)), i=1,3)]
  print *, bas5
  print *, [base:: (base(i*100,1.0/i/3,(0.0-i,i+10)), i=1,3)]


  der1 = [derived(1, 1e37, (huge(0.0),tiny(0.0)), 'a'), &
          derived(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0)), 'z'), &
          derived(-1, huge(1e3), (1e-9,1e-37), '~')]

  der2 = [derived:: derived(1, 1e37, (huge(0.0),tiny(0.0)), 'a'), &
                    derived(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0)), 'z'), &
                    derived(-1, huge(1e3), (1e-9,1e-37), '~')]

  der3 = [derived:: (derived(1, 1e37, (huge(0.0),tiny(0.0)), 'a'), &
                     derived(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0)), 'z'), &
                     derived(-1, huge(1e3), (1e-9,1e-37), '~'), i=3,3)]

  der4 = [derived:: der3]

  if (.not.(all(der1 == der2) .and. all(der2 == der3) .and. all(der3 == der4) .and. all(der4 == der1))) stop 3

  der5 = [derived:: (derived(i*100,1.0/i/3,(0.0-i,i+10), achar(64+i)), i=1,3)]
  print *, der5
  print *, [derived:: (derived(i*100,1.0/i/3,(0.0-i,i+10), achar(64+i)), i=1,3)]


  comp1 = [composed(base(-1, huge(1e3), (1e-9,1e-37)), .false.), &
           composed(base(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0))), .false.), &
           composed(base(1, 1e37, (huge(0.0),tiny(0.0))), .true.)]

  comp2 = [composed:: composed(base(-1, huge(1e3), (1e-9,1e-37)), .false.), &
                      composed(base(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0))), .false.), &
                      composed(base(1, 1e37, (huge(0.0),tiny(0.0))), .true.)]

  comp3 = [composed:: (composed(base(-1, huge(1e3), (1e-9,1e-37)), .false.), &
                       composed(base(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0))), .false.), &
                       composed(base(1, 1e37, (huge(0.0),tiny(0.0))), .true.), i=4,4)]

  comp4 = [composed:: comp3]

  if (.not.(all(comp1 == comp2) .and. all(comp2 == comp3) .and. all(comp3 == comp4) .and. all(comp4 == comp1))) stop 4

  comp5 = [composed:: (composed(base(i*100,1.0/i/3,(0.0-i,i+10)), mod(i,2) == 1), i=1,3)]
  print *, comp5
  print *, [composed:: (composed(base(i*100,1.0/i/3,(0.0-i,i+10)), mod(i,2) == 1), i=1,3)]

end program acetdt00
