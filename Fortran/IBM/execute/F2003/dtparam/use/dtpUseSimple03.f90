!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseSimple03
!*
!*  DATE                       : 2008-09-19
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : reference any module types in declarations
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  One of three tests which organise the module and the program in one of three ways:
!*  1) module and program defined in the same file,
!*  2) module defined in a different file, but INCLUDEd in the program file, and
!*  3) module and program defined in different files.
!*  In all cases, the program will USE the module and employ the types in declarations.
!*
!*  This is test #3: module and program defined in different files
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


program dtpUseSimple03

  use dtpUseSimpleMod
  implicit none
  type(base) :: b1 = base(123456789_4)
  type(kDerived(2)) :: kd1 = kDerived(2)(23456789_4,.true._2)
  type(lDerived(3)) :: ld1 = lDerived(3)(3456789_4,"sup")

  type(dtpKBase(2)) :: dkb1 = dtpKBase(2)(4321_2)
  type(dtpKDerived(2)) :: dkd1 = dtpKDerived(2)(5432_2,14321_2)
  type(dtpK2Derived(2,6)) :: dkd2 = dtpK2Derived(2,6)(1234_2,23456_2,234567891_4)

  type(dtpLBase(5)) :: dlb1 = dtpLBase(5)([1,2,3,4,5])
  type(dtpLDerived(3)) :: dld1 = dtpLDerived(3)([2,3,4],"abc")
  type(dtpL2Derived(3,9)) :: dld2 = dtpL2Derived(3,9)([2,3,4],"abc","twelvedaysof")

  print *, ">", b1, "<"
  print *, ">", kd1, "<", kd1%k, kind(kd1%icomp), kind(kd1%lcomp2)
  print *, ">", ld1, "<", ld1%l, kind(ld1%icomp), len(ld1%chComp)

  print *, ">", dkb1, "<", dkb1%k, kind(dkb1%icomp)
  print *, ">", dkd1, "<", dkd1%k, kind(dkd1%icomp), kind(dkd1%icomp2)
  print *, ">", dkd2, "<", dkd2%k, dkd2%k2, kind(dkd2%icomp), kind(dkd2%icomp2), kind(dkd2%icomp3)

  print *, ">", dlb1, "<", dlb1%l, size(dlb1%icomp)
  print *, ">", dld1, "<", dld1%l, size(dld1%icomp), len(dld1%chComp)
  print *, ">", dld2, "<", dld2%l, dld2%l2, size(dld2%icomp), len(dld2%chComp), len(dld2%chComp2)

end program dtpUseSimple03
