!******************************************************************************
!*  ===========================================================================
!*
!*  DATE                       : 2006-06-14 (YYYY-MM-DD)
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Assignment of logical A.C. to array variable
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic type, assignment, array constructor
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that assignment of a logical array constructor to a logical
!*  array variable uses the correct values.  All the values within the
!*  constructor should be converted to match the type specifier before the
!*  constructor is used to initialise the variable, whether widening or
!*  narrowing, so an assignment of the form
!*    <L*2var> = (/ logical*2:: <L*1value>, <L*2value>, <L*4value> /)
!*  (e.g., L2arr = (/ logical*2:: .true._1, .false._1, .true._2, .false._2, .true._4, .false._4 /))
!*  should leave only logical*2 values
!*  (so L2arr should be (/ T F T F T F /))
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint40la

  implicit none
  integer    :: i
  logical    :: larr (8), larra (8), larrb (8)
  logical(1) :: larr1(8), larr1a(8), larr1b(8)
  logical(2) :: larr2(8), larr2a(8), larr2b(8)
  logical(4) :: larr4(8), larr4a(8), larr4b(8)
  logical(8) :: larr8(8), larr8a(8), larr8b(8)

  logical(1), parameter :: T1 = .true._1, F1 = .false._1
  logical(2), parameter :: T2 = .true._2, F2 = .false._2
  logical(4), parameter :: T4 = .true._4, F4 = .false._4
  logical(8), parameter :: T8 = .true._8, F8 = .false._8

  ! Make sure the values in each array are correct
  !   - assigning via an array constructor makes for a circular test.
  larr1(1) = T1; larr1(2) = T2; larr1(3) = T4; larr1(4) = T8; larr1(5) = F1; larr1(6) = F2; larr1(7) = F4; larr1(8) = F8
  larr2(1) = T1; larr2(2) = T2; larr2(3) = T4; larr2(4) = T8; larr2(5) = F1; larr2(6) = F2; larr2(7) = F4; larr2(8) = F8
  larr4(1) = T1; larr4(2) = T2; larr4(3) = T4; larr4(4) = T8; larr4(5) = F1; larr4(6) = F2; larr4(7) = F4; larr4(8) = F8
  larr8(1) = T1; larr8(2) = T2; larr8(3) = T4; larr8(4) = T8; larr8(5) = F1; larr8(6) = F2; larr8(7) = F4; larr8(8) = F8
  larr(1)  = T1; larr(2)  = T2; larr(3)  = T4; larr(4)  = T8; larr(5)  = F1; larr(6)  = F2; larr(7)  = F4; larr(8)  = F8


  larr1a = (/logical(1):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr1b = (/logical(1):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr1a .eqv. larr1) .and. all(larr1b .eqv. larr1))) then
     print *, "larr1a (",larr1a,") should be (",larr1,")"
     print *, "larr1b (",larr1b,") should be (",larr1,")"
     error stop 1_4
  end if

  larr2a = (/logical(1):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr2b = (/logical(1):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr2a .eqv. larr1) .and. all(larr2b .eqv. larr1))) then
     print *, "larr2a (",larr2a,") should be (",larr1,")"
     print *, "larr2b (",larr2b,") should be (",larr1,")"
     error stop 2_4
  end if

  larr4a = (/logical(1):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr4b = (/logical(1):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr4a .eqv. larr1) .and. all(larr4b .eqv. larr1))) then
     print *, "larr4a (",larr4a,") should be (",larr1,")"
     print *, "larr4b (",larr4b,") should be (",larr1,")"
     error stop 3_4
  end if

  larr8a = (/logical(1):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr8b = (/logical(1):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr8a .eqv. larr1) .and. all(larr8b .eqv. larr1))) then
     print *, "larr8a (",larr8a,") should be (",larr1,")"
     print *, "larr8b (",larr8b,") should be (",larr1,")"
     error stop 4_4
  end if

  larra  = (/logical(1):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larrb  = (/logical(1):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larra  .eqv. larr1) .and. all(larrb  .eqv. larr1))) then
     print *, "larra (",larra,") should be (", larr1,")"
     print *, "larrb (",larrb,") should be (", larr1,")"
     error stop 5_4
  end if

  larr1a = (/logical(2):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr1b = (/logical(2):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr1a .eqv. larr1) .and. all(larr1b .eqv. larr1))) then
     print *, "larr1a (",larr1a,") should be (",larr1,")"
     print *, "larr1b (",larr1b,") should be (",larr1,")"
     error stop 6_4
  end if

  larr2a = (/logical(2):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr2b = (/logical(2):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr2a .eqv. larr2) .and. all(larr2b .eqv. larr2))) then
     print *, "larr2a (",larr2a,") should be (",larr2,")"
     print *, "larr2b (",larr2b,") should be (",larr2,")"
     error stop 7_4
  end if

  larr4a = (/logical(2):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr4b = (/logical(2):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr4a .eqv. larr2) .and. all(larr4b .eqv. larr2))) then
     print *, "larr4a (",larr4a,") should be (",larr2,")"
     print *, "larr4b (",larr4b,") should be (",larr2,")"
     error stop 8_4
  end if

  larr8a = (/logical(2):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr8b = (/logical(2):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr8a .eqv. larr2) .and. all(larr8b .eqv. larr2))) then
     print *, "larr8a (",larr8a,") should be (",larr2,")"
     print *, "larr8b (",larr8b,") should be (",larr2,")"
     error stop 9_4
  end if

  larra  = (/logical(2):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larrb  = (/logical(2):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larra  .eqv. larr2) .and. all(larrb  .eqv. larr2))) then
     print *, "larra (",larra,") should be (", larr2,")"
     print *, "larrb (",larrb,") should be (", larr2,")"
     error stop 10_4
  end if

  larr1a = (/logical(4):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr1b = (/logical(4):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr1a .eqv. larr1) .and. all(larr1b .eqv. larr1))) then
     print *, "larr1a (",larr1a,") should be (",larr1,")"
     print *, "larr1b (",larr1b,") should be (",larr1,")"
     error stop 11_4
  end if

  larr2a = (/logical(4):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr2b = (/logical(4):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr2a .eqv. larr2) .and. all(larr2b .eqv. larr2))) then
     print *, "larr2a (",larr2a,") should be (",larr2,")"
     print *, "larr2b (",larr2b,") should be (",larr2,")"
     error stop 12_4
  end if

  larr4a = (/logical(4):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr4b = (/logical(4):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr4a .eqv. larr4) .and. all(larr4b .eqv. larr4))) then
     print *, "larr4a (",larr4a,") should be (",larr4,")"
     print *, "larr4b (",larr4b,") should be (",larr4,")"
     error stop 13_4
  end if

  larr8a = (/logical(4):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr8b = (/logical(4):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr8a .eqv. larr4) .and. all(larr8b .eqv. larr4))) then
     print *, "larr8a (",larr8a,") should be (",larr4,")"
     print *, "larr8b (",larr8b,") should be (",larr4,")"
     error stop 14_4
  end if

  ! If -qintsize=2, then larra is actually logical*2 and not logical*4; this shouldn't matter, though:
  larra  = (/logical(4):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larrb  = (/logical(4):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larra .eqv. larr4) .and. all(larrb .eqv. larr4))) then
     print *, "larra (",larra,") should be (",larr4,")"
     print *, "larrb (",larrb,") should be (",larr4,")"
     error stop 15_4
  end if

  larr1a = (/logical(8):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr1b = (/logical(8):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr1a .eqv. larr1) .and. all(larr1b .eqv. larr1))) then
     print *, "larr1a (",larr1a,") should be (",larr1,")"
     print *, "larr1b (",larr1b,") should be (",larr1,")"
     error stop 16_4
  end if

  larr2a = (/logical(8):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr2b = (/logical(8):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr2a .eqv. larr2) .and. all(larr2b .eqv. larr2))) then
     print *, "larr2a (",larr2a,") should be (",larr2,")"
     print *, "larr2b (",larr2b,") should be (",larr2,")"
     error stop 17_4
  end if

  larr4a = (/logical(8):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr4b = (/logical(8):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr4a .eqv. larr4) .and. all(larr4b .eqv. larr4))) then
     print *, "larr4a (",larr4a,") should be (",larr4,")"
     print *, "larr4b (",larr4b,") should be (",larr4,")"
     error stop 18_4
  end if

  larr8a = (/logical(8):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr8b = (/logical(8):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr8a .eqv. larr8) .and. all(larr8b .eqv. larr8))) then
     print *, "larr8a (",larr8a,") should be (",larr8,")"
     print *, "larr8b (",larr8b,") should be (",larr8,")"
     error stop 19_4
  end if

  larra  = (/logical(8):: T1, T2, T4, T8, F1, F2, F4, F8/)
  larrb  = (/logical(8):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larra .eqv. larr8) .and. all(larrb .eqv. larr8))) then
     print *, "larra (",larra,") should be (",larr8,")"
     print *, "larrb (",larrb,") should be (",larr8,")"
     error stop 20_4
  end if

  larr1a = (/logical   :: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr1b = (/logical   :: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr1a .eqv. larr1) .and. all(larr1b .eqv. larr1))) then
     print *, "larr1a (",larr1a,") should be (",larr1,")"
     print *, "larr1b (",larr1b,") should be (",larr1,")"
     error stop 21_4
  end if

  larr2a = (/logical   :: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr2b = (/logical   :: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr2a .eqv. larr2) .and. all(larr2b .eqv. larr2))) then
     print *, "larr2a (",larr2a,") should be (",larr2,")"
     print *, "larr2b (",larr2b,") should be (",larr2,")"
     error stop 22_4
  end if

  larr4a = (/logical   :: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr4b = (/logical   :: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr4a .eqv. larr) .and. all(larr4b .eqv. larr))) then
     print *, "larr4a (",larr4a,") should be (",larr,")"
     print *, "larr4b (",larr4b,") should be (",larr,")"
     error stop 23_4
  end if

  larr8a = (/logical   :: T1, T2, T4, T8, F1, F2, F4, F8/)
  larr8b = (/logical   :: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larr8a .eqv. larr) .and. all(larr8b .eqv. larr))) then
     print *, "larr8a (",larr8a,") should be (",larr,")"
     print *, "larr8b (",larr8b,") should be (",larr,")"
     error stop 24_4
  end if

  larra = (/logical   :: T1, T2, T4, T8, F1, F2, F4, F8/)
  larrb = (/logical   :: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  if (.not.(all(larra .eqv. larr) .and. all(larrb .eqv. larr))) then
     print *, "larra (",larra,") should be (",larr,")"
     print *, "larrb (",larrb,") should be (",larr,")"
     error stop 25_4
  end if

end program acetint40la
