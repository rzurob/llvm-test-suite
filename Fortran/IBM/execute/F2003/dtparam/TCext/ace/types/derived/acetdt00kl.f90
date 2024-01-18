!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2007-12-07 (original: 2006-09-27)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*                               (+ Array Constructor Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement derived TS,
!*                               no parameters, simple objects of correct type
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
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
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt00mod

  use, intrinsic :: ieee_arithmetic
  implicit none
  type :: base (kbase_1,kbase_2,kbase_3) ! kbase_1,kbase_2,kbase_3=4,4,4
     integer, kind :: kbase_1,kbase_2,kbase_3
     integer(kbase_1) :: int
     real(kbase_1)    :: rl
     complex(kbase_1) :: z
   contains
     procedure :: check_equals => base_equals
     generic :: operator(.eq.) => check_equals
  end type base

  type, extends (base) :: derived (lderived_1) ! lderived_1=1
     integer, len :: lderived_1
     character (lderived_1):: ch
   contains
     procedure :: check_equals => derived_equals
!     generic :: operator(.eq.) => derived_equals
  end type derived

  type :: composed (kcomposed_1,kcomposed_2) ! kcomposed_1,kcomposed_2=1,4
     integer, kind :: kcomposed_1,kcomposed_2
     type (base(kcomposed_2,kcomposed_2,kcomposed_2))  :: b1 ! tcx: (kcomposed_2,kcomposed_2,kcomposed_2)
     logical(kcomposed_1)   :: log
   contains
     procedure :: composed_equals
     generic :: operator(.eq.) => composed_equals
  end type composed

contains

  elemental logical function base_equals(a, b)
    class (base(4,4,4)), intent(in) :: a, b ! tcx: (4,4,4)
    base_equals = (a%int == b%int) .and. isSameReal4(a%rl,b%rl) .and. isSameComplex4(a%z,b%z)
  end function base_equals

  elemental logical function derived_equals(a, b)
    class (derived(4,4,4,*)), intent(in) :: a ! tcx: (4,4,4,*)
    class (base(4,4,4)), intent(in) :: b ! tcx: (4,4,4)

    select type (b)
        class is (derived(4,4,4,*)) ! tcx: (4,4,4,*)
            derived_equals = base_equals(a%base,b%base) .and. (a%ch == b%ch)

        class default
            derived_equals = .false.
    end select
  end function derived_equals

  elemental logical function composed_equals(a, b)
    class (composed(1,4)), intent(in) :: a, b ! tcx: (1,4)
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


program acetdt00kl

  use acetdt00mod
  implicit none

  type(base(4,4,4))     :: bas1(3), bas2(3), bas3(3), bas4(3), bas5(3) ! tcx: (4,4,4)
  type(derived(4,4,4,1))  :: der1(3), der2(3), der3(3), der4(3), der5(3) ! tcx: (4,4,4,1)
  type(composed(1,4)) :: comp1(3), comp2(3), comp3(3), comp4(3), comp5(3) ! tcx: (1,4)
  integer        :: i


  bas1 = [base(4,4,4)(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0))), & ! tcx: (4,4,4)
          base(4,4,4)(1, 1e37, (huge(0.0),tiny(0.0))), & ! tcx: (4,4,4)
          base(4,4,4)(-1, huge(1e3), (1e-9,1e-37))] ! tcx: (4,4,4)

  bas2 = [base(4,4,4):: base(4,4,4)(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0))), & ! tcx: (4,4,4) ! tcx: (4,4,4)
                 base(4,4,4)(1, 1e37, (huge(0.0),tiny(0.0))), & ! tcx: (4,4,4)
                 base(4,4,4)(-1, huge(1e3), (1e-9,1e-37))] ! tcx: (4,4,4)

  bas3 = [base(4,4,4):: (base(4,4,4)(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0))), & ! tcx: (4,4,4) ! tcx: (4,4,4)
                  base(4,4,4)(1, 1e37, (huge(0.0),tiny(0.0))), & ! tcx: (4,4,4)
                  base(4,4,4)(-1, huge(1e3), (1e-9,1e-37)), i=2,2)] ! tcx: (4,4,4)

  bas4 = [base(4,4,4):: bas3] ! tcx: (4,4,4)

  if (.not.(all(bas1 == bas2) .and. all(bas2 == bas3) .and. all(bas3 == bas4) .and. all(bas4 == bas1))) stop 2

  bas5 = [base(4,4,4):: (base(4,4,4)(i*100,1.0/i/3,(0.0-i,i+10)), i=1,3)] ! tcx: (4,4,4) ! tcx: (4,4,4)
  print *, bas5
  print *, [base(4,4,4):: (base(4,4,4)(i*100,1.0/i/3,(0.0-i,i+10)), i=1,3)] ! tcx: (4,4,4) ! tcx: (4,4,4)


  der1 = [derived(4,4,4,1)(1, 1e37, (huge(0.0),tiny(0.0)), 'a'), & ! tcx: (4,4,4,1)
          derived(4,4,4,1)(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0)), 'z'), & ! tcx: (4,4,4,1)
          derived(4,4,4,1)(-1, huge(1e3), (1e-9,1e-37), '~')] ! tcx: (4,4,4,1)

  der2 = [derived(4,4,4,1):: derived(4,4,4,1)(1, 1e37, (huge(0.0),tiny(0.0)), 'a'), & ! tcx: (4,4,4,1) ! tcx: (4,4,4,1)
                    derived(4,4,4,1)(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0)), 'z'), & ! tcx: (4,4,4,1)
                    derived(4,4,4,1)(-1, huge(1e3), (1e-9,1e-37), '~')] ! tcx: (4,4,4,1)

  der3 = [derived(4,4,4,1):: (derived(4,4,4,1)(1, 1e37, (huge(0.0),tiny(0.0)), 'a'), & ! tcx: (4,4,4,1) ! tcx: (4,4,4,1)
                     derived(4,4,4,1)(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0)), 'z'), & ! tcx: (4,4,4,1)
                     derived(4,4,4,1)(-1, huge(1e3), (1e-9,1e-37), '~'), i=3,3)] ! tcx: (4,4,4,1)

  der4 = [derived(4,4,4,1):: der3] ! tcx: (4,4,4,1)

  if (.not.(all(der1 == der2) .and. all(der2 == der3) .and. all(der3 == der4) .and. all(der4 == der1))) stop 3

  der5 = [derived(4,4,4,1):: (derived(4,4,4,1)(i*100,1.0/i/3,(0.0-i,i+10), achar(64+i)), i=1,3)] ! tcx: (4,4,4,1) ! tcx: (4,4,4,1)
  print *, der5
  print *, [derived(4,4,4,1):: (derived(4,4,4,1)(i*100,1.0/i/3,(0.0-i,i+10), achar(64+i)), i=1,3)] ! tcx: (4,4,4,1) ! tcx: (4,4,4,1)


  comp1 = [composed(1,4)(base(4,4,4)(-1, huge(1e3), (1e-9,1e-37)), .false.), & ! tcx: (4,4,4) ! tcx: (1,4)
           composed(1,4)(base(4,4,4)(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0))), .false.), & ! tcx: (4,4,4) ! tcx: (1,4)
           composed(1,4)(base(4,4,4)(1, 1e37, (huge(0.0),tiny(0.0))), .true.)] ! tcx: (4,4,4) ! tcx: (1,4)

  comp2 = [composed(1,4):: composed(1,4)(base(4,4,4)(-1, huge(1e3), (1e-9,1e-37)), .false.), & ! tcx: (4,4,4) ! tcx: (1,4) ! tcx: (1,4)
                      composed(1,4)(base(4,4,4)(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0))), .false.), & ! tcx: (4,4,4) ! tcx: (1,4)
                      composed(1,4)(base(4,4,4)(1, 1e37, (huge(0.0),tiny(0.0))), .true.)] ! tcx: (4,4,4) ! tcx: (1,4)

  comp3 = [composed(1,4):: (composed(1,4)(base(4,4,4)(-1, huge(1e3), (1e-9,1e-37)), .false.), & ! tcx: (4,4,4) ! tcx: (1,4) ! tcx: (1,4)
                       composed(1,4)(base(4,4,4)(huge(1), 1e-9, 1/(huge(0.0),tiny(0.0))), .false.), & ! tcx: (4,4,4) ! tcx: (1,4)
                       composed(1,4)(base(4,4,4)(1, 1e37, (huge(0.0),tiny(0.0))), .true.), i=4,4)] ! tcx: (4,4,4) ! tcx: (1,4)

  comp4 = [composed(1,4):: comp3] ! tcx: (1,4)

  if (.not.(all(comp1 == comp2) .and. all(comp2 == comp3) .and. all(comp3 == comp4) .and. all(comp4 == comp1))) stop 4

  comp5 = [composed(1,4):: (composed(1,4)(base(4,4,4)(i*100,1.0/i/3,(0.0-i,i+10)), mod(i,2) == 1), i=1,3)] ! tcx: (4,4,4) ! tcx: (1,4) ! tcx: (1,4)
  print *, comp5
  print *, [composed(1,4):: (composed(1,4)(base(4,4,4)(i*100,1.0/i/3,(0.0-i,i+10)), mod(i,2) == 1), i=1,3)] ! tcx: (4,4,4) ! tcx: (1,4) ! tcx: (1,4)

end program acetdt00kl


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1,kbase_2,kbase_3) to invoke with (4,4,4)/declare with (4,4,4) - 31 changes
! type: derived - added parameters (lderived_1) to invoke with (4,4,4,1)/declare with (4,4,4,*) - 19 changes
! type: composed - added parameters (kcomposed_1,kcomposed_2) to invoke with (1,4)/declare with (1,4) - 18 changes
