!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-01-21
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment without Lower Bounds Specification or Remap
!*
!*  SECONDARY FUNCTIONS TESTED : pointer use within ASSOCIATE construct
!*
!*  REFERENCE                  : Feature Number 360669
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Declare targets of fixed length parameters and pointers with deferred length
!*  parameters, and use the pointers as selectors in ASSOCIATE constructs, verifying
!*  that the parameters and values accessed through the associate-name are correct.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPtrAssignAssociateMod
  type dk(k)
     integer, kind :: k
     integer(k) :: idat = 0
  end type dk

  type dl(l)
     integer, len  :: l
     character(l) :: cdat = "----"
  end type dl

  type dboth (k, l)
     integer, kind :: k
     integer, len  :: l
     type (dk(k)) :: dk1
     type (dk(k)) :: dk2
     type (dk(k)), pointer :: dkp  => null()
     type (dl(l)) :: dl1
     type (dl(l)) :: dl2
     type (dl(l)), pointer :: dlp  => null()
     type (dl(:)), pointer :: dlqp => null()
  end type dboth

  type dcontainer (k, l)
     integer, kind :: k
     integer, len  :: l
     type(dboth(k,l)) :: db
     type(dboth(k,l)), pointer :: dbp  => null()
     type(dboth(k,:)), pointer :: dbqp => null()
  end type dcontainer

  integer :: eCount = 0

contains

  subroutine checkIVal(n1, i1, n2, i2)
    character(*), intent(in) :: n1, n2
    integer, intent(in) :: i1, i2
    if (i1 /= i2) then
       print *, "I: ", n1, " (", i1, ") /= ", n2, " (", i2, ")"
       eCount = eCount + 1
    end if
  end subroutine checkIVal

  subroutine checkCVal(n1, c1, n2, c2)
    character(*), intent(in) :: n1, n2, c1, c2
    if (c1 /= c2) then
       print *, "C: ", n1, " ('", c1, "') /= ", n2, " ('", c2, "')"
       eCount = eCount + 1
    end if
  end subroutine checkCVal

end module dtpPtrAssignAssociateMod

program dtpPtrAssignAssociate

  use :: dtpPtrAssignAssociateMod
  implicit none

  type(dk(4)), pointer :: pk4, pk4a
  type(dk(8)), pointer :: pk8, pk8a
  type(dk(8)), target  :: tk8
  type(dk(4)), target  :: tk4, tk4a

  type(dl(4)), pointer :: pl4, pl4a
  type(dl(8)), pointer :: pl8, pl8a
  type(dl(:)), pointer :: plq, plqa
  type(dl(8)), target  :: tl8
  type(dl(4)), target  :: tl4

  type(dboth(4,4)), pointer :: pb44, pb44a
  type(dboth(4,8)), pointer :: pb48, pb48a
  type(dboth(4,:)), pointer :: pb4q, pb4qa
  type(dboth(8,4)), pointer :: pb84, pb84a
  type(dboth(8,8)), pointer :: pb88, pb88a
  type(dboth(8,:)), pointer :: pb8q, pb8qa

  type(dboth(4,4)), target  :: tb44, tb44a
  type(dboth(4,8)), target  :: tb48
  type(dboth(8,4)), target  :: tb84
  type(dboth(8,8)), target  :: tb88

  integer(4), pointer :: ip4
  logical :: parametersFail = .false., valuesFail = .false.

  interface check
     module procedure checkIVal
     module procedure checkCVal
  end interface

  tk4 = dk(4)(103)
  tl4 = dl(4)("ijkl")
  pl4 => tl4

                   ! dk1,       dk2,       dkp,dl1,          dl2,          dlp,dlqp
  tb44 = dboth(4,4)(dk(4)(101), dk(4)(102),tk4,dl(4)("abcd"),dl(4)("efgh"),pl4,pl4)
  tb44a = tb44

  pb4q => tb44
  associate(var => pb4q)
    ! var should now be a name for the object referenced by pb4q, but *not* a pointer
    ! check lengths and kinds
    call check("tb44 % k", tb44 % k, "var % k", var % k)
    call check("tb44 % l", tb44 % l, "var % l", var % l)
    call check("tb44 % dk1 % k", tb44 % dk1 % k, "var % dk1 % k", var % dk1 % k)
    call check("tb44 % dk2 % k", tb44 % dk2 % k, "var % dk2 % k", var % dk2 % k)
    call check("tb44 % dkp % k", tb44 % dkp % k, "var % dkp % k", var % dkp % k)
    call check("tb44 % dl1 % l", tb44 % dl1 % l, "var % dl1 % l", var % dl1 % l)
    call check("tb44 % dl2 % l", tb44 % dl2 % l, "var % dl2 % l", var % dl2 % l)
    call check("tb44 % dlp % l", tb44 % dlp % l, "var % dlp % l", var % dlp % l)
    call check("tb44 % dlqp % l", tb44 % dlqp % l, "var % dlqp % l", var % dlqp % l)
    ! check data values
    call check("tb44 % dk1 % idat", tb44 % dk1 % idat, "var % dk1 % idat", var % dk1 % idat)
    call check("tb44 % dk2 % idat", tb44 % dk2 % idat, "var % dk2 % idat", var % dk2 % idat)
    call check("tb44 % dkp % idat", tb44 % dkp % idat, "var % dkp % idat", var % dkp % idat)
    call check("tb44 % dl1 % cdat", tb44 % dl1 % cdat, "var % dl1 % cdat", var % dl1 % cdat)
    call check("tb44 % dl2 % cdat", tb44 % dl2 % cdat, "var % dl2 % cdat", var % dl2 % cdat)
    call check("tb44 % dlp % cdat", tb44 % dlp % cdat, "var % dlp % cdat", var % dlp % cdat)
    call check("tb44 % dlqp % cdat", tb44 % dlqp % cdat, "var % dlqp % cdat", var % dlqp % cdat)
    if (eCount > 0) error stop 2
    print *, "passed: ", var ! print out data
  end associate

end program dtpPtrAssignAssociate
