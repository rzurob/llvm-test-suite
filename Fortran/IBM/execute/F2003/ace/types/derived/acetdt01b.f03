!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-09-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : no-parameter derived TS pointer component with defined assignment via elementals
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : derived type, defined assignment, elemental, component
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Like acetdt01, we test defined assignment, but we do not define assignment
!*  for the type of the top-level structures (here, type pContainer), but for
!*  the type of a component (type: dt).  While acetdt01a shows the use of
!*  such components included in the structure, here we show the use of pointers
!*  to these components.  Since the assignments involve pointers, we should
!*  see almost no use of defined assignment, but rather of pointer assignment.
!*  So, if assignment is user-defined for type S, and one of the components of
!*  type T is a pointer to an object of type S, then we should see defined
!*  assignment being invoked only in:
!*
!*      s1 = s2
!*      s1 = s(val)
!*      t1%sField = s2
!*
!*  but not in:
!*
!*      t2 = t1
!*      t1array = t1
!*      t2array = t1array
!*      t1array = (/ t1 /)
!*      t2array = (/ t1array /)
!*  (Normally, we'd also consider "t1 = t(s(val))", but the case is invalid,
!*  since we would need the structure constructor ("s(val)") to be a target.)
!*
!*  There are two ways in which defined assignment can be set up:
!*
!*  1. by use of a generic binding to assignment(=) in the type definition,
!*     using type-bound procedures and passed-object dummy arguments, or
!*  2. by use of a generic interface and procedures (type-bound or otherwise)
!*     which do not have passed-object dummy arguments.
!*
!*  Note that defined assignment is only invoked on components if the first
!*  route is used (and even then, not if the field is a pointer).
!*
!*  C461 requires specific bindings for assignment(=) to have a passed-object
!*  dummy argument, which must be "a scalar, nonpointer, nonallocatable dummy
!*  data object", according to C453.  This means that we cannot define
!*  assignment for arrays except by using either *elemental* type-bound procedures
!*  with passed-object dummy arguments (so no print statements, etc.), or
!*  nopass procedures with dummy array arguments and a generic interface.
!*
!*  Here we test type-bound procedures with passed-object dummy arguments,
!*  meaning we use elemental procedures, and cannot print trace output, but
!*  we can show that defined assignment has been used if we assign a different
!*  value than would be assigned by intrinsic assignment, e.g., by first doubling
!*  the value.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt01bmod

  implicit none

  type dt
     integer :: datum  = -1
   contains
     procedure :: dtAssignDt
     generic   :: assignment(=) => dtAssignDt
  end type dt

  type pContainer
     integer :: id = 0
     type (dt), pointer :: d
  end type pContainer

contains

  elemental subroutine dtAssignDt(this, that)
    class (dt), intent(inout) :: this
    class (dt), intent(in) :: that
    this % datum = 2 * that % datum
  end subroutine dtAssignDt

  subroutine init(pc, dtv, id, datum)
    class (pContainer), intent(inout) :: pc
    class (dt), intent(inout), target :: dtv
    integer, intent(in) :: id, datum
    pc % id = id
    dtv % datum = datum
    pc % d => dtv
  end subroutine init

end module acetdt01bmod


program acetdt01b

  use acetdt01bmod
  implicit none
  type (dt), target :: dt1, dt2, dt3, dt4, dt5, dt6, dt7, dt8, dt9
  type (pContainer) :: pc, pc2
  type (pContainer), target :: pca(2), pca2(2)
  type (pContainer), allocatable :: pcall(:)
  type (pContainer), pointer :: pcp(:)
  integer :: before, i

  ! Set up arrays:
  call init(pc,      dt1, 1, 101)
  call init(pc2,     dt2, 2, 102)
  call init(pca(1),  dt3, 3, 103)
  call init(pca(2),  dt4, 4, 104)
  call init(pca2(1), dt5, 5, 105)
  call init(pca2(2), dt6, 6, 106)

  ! Verify that defined assignment does work, if correctly invoked:
  dt7 % datum = 107
  dt7 = dt1
  if (dt7%datum /= (dt1%datum * 2)) error stop 2

  dt7 = dt(110)
  if (dt7%datum /= 220) error stop 2

  before = pc%d%datum
  pc%d = dt(111)
  if (pc%d%datum /= 222) error stop 2
  pc%d%datum = before

  ! but not anywhere else:

  before = dt7 % datum
  pc   = pContainer(8, dt7)  ! pointer assign, not defined assign
  if (pc%d%datum /= before) error stop 3

  !* t1 = t(s(val)) -- not legal for pointers
  ! now shuffle values a little; because the dt field is a pointer, defined assignment should not kick in:
  pc2  = pc
  if (pc2%d%datum /= pc%d%datum) error stop 4

  pca  = pc2
  if (pca(1)%d%datum /= pc2%d%datum .or. pca(2)%d%datum /= pc2%d%datum) error stop 5

  pca2 = pca
  if  (pca2(1)%d%datum /= pca(1)%d%datum .or. pca2(2)%d%datum /= pca(2)%d%datum) error stop 6

  ! That was all preamble - making sure that things were initialised and that
  ! defined assignment happened when expected.  Now for the real tests:

  ! Reinitialize pca:
  call init(pca(1),  dt3, 3, 103)
  call init(pca(2),  dt4, 4, 104)

  pca  = (/ pc, pc2 /)
  if (pca(1)%d%datum /= pc%d%datum .or. pca(2)%d%datum /= pc2%d%datum) error stop 7

  ! Reinitialize pca2:
  call init(pca2(1), dt5, 5, 105)
  call init(pca2(2), dt6, 6, 106)
  pca2 = (/ pca /)
  if  (pca2(1)%d%datum /= pca(1)%d%datum .or. pca2(2)%d%datum /= pca(2)%d%datum) error stop 8

  call init(pca(1),  dt3, 3, 103)
  call init(pca(2),  dt4, 4, 104)
  pca  = (/ pContainer:: pc, pc2 /)
  if (pca(1)%d%datum /= pc%d%datum .or. pca(2)%d%datum /= pc2%d%datum) error stop 9

  call init(pca2(1), dt5, 5, 105)
  call init(pca2(2), dt6, 6, 106)
  pca2 = (/ pContainer:: pca /)
  if (pca2(1)%d%datum /= pca(1)%d%datum .or. pca2(2)%d%datum /= pca(2)%d%datum) error stop 10

  allocate(pcall(2))
  call init(pcall(1), dt8, 11, 121)
  call init(pcall(2), dt9, 12, 122)

  pca = (/ pContainer:: pcall /)
  if (pca(1)%d%datum /= pcall(1)%d%datum .or. pca(2)%d%datum /= pcall(2)%d%datum) error stop 11

  pcall = (/ pContainer:: pca2 /)
  if (pcall(1)%d%datum /= pca2(1)%d%datum .or. pcall(2)%d%datum /= pca2(2)%d%datum) error stop 12

  pcp => pca
  pcp = (/ pContainer:: pcall /)
  if (pcp(1)%d%datum /= pcall(1)%d%datum .or. pcp(2)%d%datum /= pcall(2)%d%datum) error stop 13

end program acetdt01b
