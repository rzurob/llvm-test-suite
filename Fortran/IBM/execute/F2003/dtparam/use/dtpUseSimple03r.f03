!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-09-19
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : reference any renamed module types in declarations
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
!*  An extension of the simple tests that renames all classes defined in the module
!*  and attempts to use them as planned with the standard three tests already established:
!*  1) module and program defined in the same file,
!*  2) module defined in a different file, but INCLUDED in the program file, and
!*  3) module and program defined in different files.
!*
!*  This is test #3: module and program defined in different files
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


program dtpUseSimple03r

  use :: dtpUseSimpleMod, cl1 => base, cl2 => kDerived, cl3 => lDerived, &
       cl4 => dtpKBase, cl5 => dtpKDerived, cl6 => dtpK2Derived, &
       cl7 => dtpLBase, cl8 => dtpLDerived, cl9 => dtpL2Derived

  implicit none
  type(cl1) :: b1 = cl1(123456789_4)
  type(cl2(2)) :: kd1 = cl2(2)(23456789_4,.true._2)
  type(cl3(3)) :: ld1 = cl3(3)(3456789_4,"sup")

  type(cl4(2)) :: dkb1 = cl4(2)(4321_2)
  type(cl5(2)) :: dkd1 = cl5(2)(5432_2,14321_2)
  type(cl6(2,6)) :: dkd2 = cl6(2,6)(1234_2,23456_2,234567891_4)

  type(cl7(5)) :: dlb1 = cl7(5)([1,2,3,4,5])
  type(cl8(3)) :: dld1 = cl8(3)([2,3,4],"abc")
  type(cl9(3,9)) :: dld2 = cl9(3,9)([2,3,4],"abc","twelvedaysof")

  print *, ">", b1, "<"
  print *, ">", kd1, "<", kd1%k, kind(kd1%icomp), kind(kd1%lcomp2)
  print *, ">", ld1, "<", ld1%l, kind(ld1%icomp), len(ld1%chComp)

  print *, ">", dkb1, "<", dkb1%k, kind(dkb1%icomp)
  print *, ">", dkd1, "<", dkd1%k, kind(dkd1%icomp), kind(dkd1%icomp2)
  print *, ">", dkd2, "<", dkd2%k, dkd2%k2, kind(dkd2%icomp), kind(dkd2%icomp2), kind(dkd2%icomp3)

  print *, ">", dlb1, "<", dlb1%l, size(dlb1%icomp)
  print *, ">", dld1, "<", dld1%l, size(dld1%icomp), len(dld1%chComp)
  print *, ">", dld2, "<", dld2%l, dld2%l2, size(dld2%icomp), len(dld2%chComp), len(dld2%chComp2)

end program dtpUseSimple03r
