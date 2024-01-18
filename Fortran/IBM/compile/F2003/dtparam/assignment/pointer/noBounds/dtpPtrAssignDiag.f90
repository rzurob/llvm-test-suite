!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignDiag
!*
!*  DATE                       : 2009-01-21
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment without Lower Bounds Specification or Remap
!*
!*  SECONDARY FUNCTIONS TESTED : diagnostic - verify type agreement on pointer assignment
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
!*  Declare targets of fixed length parameters and pointers with fixed
!*  and deferred length parameters, and try assigning different types.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program dtpPtrAssignDiag

  type dk(k)
     integer, kind :: k
     integer(k) :: idat
  end type dk

  type dl(l)
     integer, len  :: l
  end type dl

  type dboth (k, l)
     integer, kind :: k
     integer, len  :: l
     type (dk(k)) :: dk1
     type (dk(k)) :: dk2
     type (dl(l)) :: dl1
     type (dl(l)) :: dl2
  end type dboth

  type(dk(4)), pointer :: pk4, pk4a
  type(dk(8)), pointer :: pk8, pk8a
  type(dk(8)), target  :: tk8
  type(dk(4)), target  :: tk4

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

  type(dboth(4,4)), target  :: tb44
  type(dboth(4,8)), target  :: tb48
  type(dboth(8,4)), target  :: tb84
  type(dboth(8,8)), target  :: tb88

  integer(4), pointer :: ip4

  pk4  => tk4 ! okay
  pk8  => tk8 ! okay
  pk4a => pk4 ! okay
  pk8a => pk8 ! okay

  pk4  => pk8 ! error
  pk8  => tk4 ! error
  pk4  => tk8 ! error
  pk8  => pk4 ! error

  pl4  => tl4 ! okay
  pl8  => tl8 ! okay
  pl4a => pl4 ! okay
  pl8a => pl8 ! okay
  plq  => pl4 ! okay
  plqa => plq ! okay
  plq  => pl8 ! okay

  pl4  => pl8 ! error
  pl8  => tl4 ! error
  pl4  => tl8 ! error
  pl8  => pl4 ! error
  pl4  => plq ! error
  pl8  => plq ! error

  pb44  => pb44a! okay
  pb44  => tb44 ! okay
  pb48  => tb48 ! okay
  pb48  => pb48a! okay
  pb4q  => tb44 ! okay
  pb4q  => tb48 ! okay
  pb4q  => pb4qa! okay
  pb84  => tb84 ! okay
  pb84  => pb84a! okay
  pb88  => tb88 ! okay
  pb88  => pb88a! okay
  pb8q  => tb84 ! okay
  pb8q  => tb88 ! okay
  pb8q  => pb8qa! okay

  pb44  => pb48 ! error
  pb48  => tb44 ! error
  pb44  => tb48 ! error
  pb48  => pb44 ! error
  pb44  => pb4q ! error
  pb48  => pb4q ! error

  pb84  => pb88 ! error
  pb88  => tb84 ! error
  pb84  => tb88 ! error
  pb88  => pb84 ! error
  pb84  => pb8q ! error
  pb88  => pb8q ! error

  ip4  => tk4%idat ! okay
  ip4  => tk8%idat ! error

  pk4  => tb44 % dk1 ! okay
  pk4a => tb48 % dk2 ! okay

  pk8  => tb44 % dk1 ! error
  pk8a => tb48 % dk2 ! error

  plqa => tb48 % dl2 ! okay

end program dtpPtrAssignDiag
