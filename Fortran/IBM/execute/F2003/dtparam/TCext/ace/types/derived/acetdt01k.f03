!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2007-12-07 (original: 2006-09-05)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*                               (+ Array Constructor Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement no-parameter
!*                               derived TS with defined assignment via
!*                               elementals
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : derived type, defined assignment, elemental
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Defined assignment for a type T kicks in whenever the right kind of value is
!*  assigned to an object of type T *except* in an array constructor.  Thus, we
!*  should see defined assignment being invoked in:
!*
!*      t1 = t(val)
!*      t2 = t1
!*      t1array = t1
!*      t2array = t1array
!*
!*  but not *within* the array constructors below:
!*
!*      t1array = (/ t1 /)
!*      t2array = (/ t1array /)
!*
!*  The tricky part is that defined assignment will be invoked when the
!*  constructed arrays are assigned to the variables!  So, we have to
!*  distinguish between the two times.
!*
!*  There are two ways in which defined assignment can be set up:
!*
!*  1. by use of a generic binding to assignment(=) in the type definition,
!*     using type-bound procedures and passed-object dummy arguments, or
!*  2. by use of a generic interface and no generic binding in the type
!*     definition
!*
!*  Whether or not the procedures are type-bound and whether or not any
!*  type-bound procedures have a passed-object dummy argument is irrelevant
!*  if a generic interface is used in place of a generic binding in the type
!*  definition.  All of these variations will allow defined assignment
!*  at top level, i.e., "a = b".  Note that defined assignment is only
!*  invoked on components if the first route is used, i.e., if "a" and "b"
!*  above have components for which assignment is user-defined.
!*
!*  C461 requires specific bindings for assignment(=) to have a passed-object
!*  dummy argument, which must be "a scalar, nonpointer, nonallocatable dummy
!*  data object", according to C453.  This means that we cannot define
!*  assignment for arrays except by using either *elemental* type-bound
!*  procedures with passed-object dummy arguments (so no print statements,
!*  etc.), or nopass procedures with dummy array arguments and a generic
!*  interface.
!*
!*  Here we test the first way: type-bound procedures with passed-object dummy
!*  arguments, meaning we use elemental procedures, and cannot print trace
!*  output, but we can show that defined assignment has been used if we assign a
!*  different value than would be assigned by intrinsic assignment, e.g., by
!*  first doubling the value.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt01mod

  implicit none

  type derived (kderived_1) ! kderived_1=4
     integer, kind :: kderived_1
     integer(kderived_1) :: datum  = -1
   contains
     procedure :: dtAssignDt
     procedure :: dtAssignInt
     generic   :: assignment(=) => dtAssignDt, dtAssignInt
  end type derived

contains

  elemental subroutine dtAssignDt(this, that)
    class (derived(4)), intent(inout) :: this ! tcx: (4)
    class (derived(4)), intent(in) :: that ! tcx: (4)
    this % datum = that % datum * 2
  end subroutine dtAssignDt

  elemental subroutine dtAssignInt(this, that)
    class (derived(4)), intent(inout) :: this ! tcx: (4)
    integer, intent(in) :: that
    this % datum = that + 40
  end subroutine dtAssignInt

  subroutine init(this, datum)
    class (derived(4)), intent(inout) :: this ! tcx: (4)
    integer, intent(in) :: datum
    this % datum = datum
  end subroutine init

end module acetdt01mod


program acetdt01k

  use acetdt01mod
  implicit none
  type (derived(4)) :: dt1, dt2, dtarr(2), dtarr2(2) ! tcx: (4)
  type (derived(4)), target :: dtt ! tcx: (4)
  type (derived(4)), pointer :: dtp ! tcx: (4)
  type (derived(4)), allocatable :: dta, dtarra(:) ! tcx: (4)
  integer :: i, before(2)

  dtp => dtt
  allocate(dta)
  allocate(dtarra(2))

  dt1 = 1
  if (dt1%datum /= 41) error stop 2

  dt2 = 2
  if (dt2%datum /= 42) error stop 3

  dtt = 3
  if (dtt%datum /= 43) error stop 4

  dta = 4
  if (dta%datum /= 44) error stop 5

  dtp = 5
  if (dtp%datum /= 45) error stop 6

  dtarr(1) = 6
  if (dtarr(1)%datum /= 46) error stop 7

  dtarra(2) = 7
  if (dtarra(2)%datum /= 47) error stop 8

  dtarr = 8
  if (any(dtarr%datum /= 48)) error stop 9

  dtarra= 9
  if (any(dtarra%datum /= 49)) error stop 10


  dtarr = (/50, 51/)
  if (any(dtarr%datum /= (/50, 51/) + 40)) error stop 11

  dtarra = (/52, 53/)
  if (any(dtarra %datum /= (/52, 53/) + 40)) error stop 12

  dtarr2 = (/ integer:: 54, 55/)
  if (any(dtarr2 % datum /= ((/ 54, 55/) + 40))) error stop 13

  dtarra = (/ integer:: 56, 57/)
  if (any(dtarra % datum /= ((/ 56, 57/) + 40))) error stop 14


  dtarr = (/ derived(4)(58), derived(4)(59) /) ! tcx: (4) ! tcx: (4)
  if (any(dtarr % datum /= ((/ 58, 59 /) * 2))) error stop 15

  dtarr2 = (/ derived(4):: derived(4)(60), derived(4)(61) /) ! tcx: (4) ! tcx: (4) ! tcx: (4)
  if (any(dtarr2 % datum /= ((/ 60, 61 /) * 2))) error stop 16

  dtarra = (/ derived(4)(datum=60), derived(4)(61) /) ! tcx: (4) ! tcx: (4)
  if (any(dtarra % datum /= ((/ 60, 61 /) * 2))) error stop 17

  dtarra = (/ derived(4):: derived(4)(datum=62), derived(4)(63) /) ! tcx: (4) ! tcx: (4) ! tcx: (4)
  if (any(dtarra % datum /= ((/ 62, 63 /) * 2))) error stop 18


  dt1 = dt2
  if (dt1 % datum /= (dt2%datum * 2)) error stop 19

  dtarr2(1) = dtarr(2)
  if (dtarr2(1) % datum /= (dtarr(2)%datum * 2)) error stop 20


  dtarr = (/ dt1, dtt /)
  if (any(dtarr % datum /= ((/ dt1%datum, dtt%datum /) * 2))) error stop 21

  dtarr = (/ dtp, dta /)
  if (any(dtarr % datum /= ((/ dtp%datum, dta%datum /) * 2))) error stop 22

  dtarra = (/ dt2, dta /)
  if (any(dtarra % datum /= ((/ dt2%datum, dta%datum /) * 2))) error stop 23

  dtarr2 = (/ dtarr /)
  if (any(dtarr2 % datum /= (dtarr%datum * 2))) error stop 24

  dtarra = (/ dtarr2 /)
  if (any(dtarra % datum /= (dtarr2%datum * 2))) error stop 25

  dtarr = (/ derived(4):: dt1, dtt /) ! tcx: (4)
  if (any(dtarr % datum /= ((/ dt1%datum, dtt%datum /) * 2))) error stop 26

  dtarr = (/ derived(4):: dtp, dta /) ! tcx: (4)
  if (any(dtarr % datum /= ((/ dtp%datum, dta%datum /) * 2))) error stop 27

  dtarra = (/ derived(4):: dta, dtp /) ! tcx: (4)
  if (any(dtarra % datum /= ((/ dta%datum, dtp%datum /) * 2))) error stop 28

  dtarr = (/ derived(4):: dtarr2 /) ! tcx: (4)
  if (any(dtarr % datum /= (dtarr2%datum * 2))) error stop 29

  dtarra = (/ derived(4):: dtarr2 /) ! tcx: (4)
  if (any(dtarra % datum /= (dtarr2%datum * 2))) error stop 30

  before = dtarr2 % datum
  dtarr2 = (/ dtarr2 /)
  if (any(dtarr2 % datum /= (before * 2))) error stop 31

  before = dtarr2 % datum
  dtarr2 = (/ derived(4):: dtarr2 /) ! tcx: (4)
  if (any(dtarr2 % datum /= (before * 2))) error stop 32

  dtarra = (/ [ [ dtarr2 ] ] /)
  if (any(dtarra%datum /= (dtarr2%datum * 2))) error stop 33

  before = [dt2%datum, dtp%datum]
  dtarra = (/ derived(4):: [ [ dt2, dtp ] ] /) ! tcx: (4)
  if (any(dtarra%datum /= (before * 2))) error stop 34

  dtarra = (/ [ [ (dtarr2(i),i=1,2) ] ] /)
  if (any(dtarra%datum /= (dtarr2%datum * 2))) error stop 35

  before = [ (dtp%datum,i=1,1), (dt2%datum,i=1,1) ]
  dtarra = (/ derived(4):: [ [ (dtp,i=1,1), (dt2,i=1,1) ] ] /) ! tcx: (4)
  if (any(dtarra%datum /= (before * 2))) error stop 36

  !!Prior to a Feb6 (?) interp, the standard was broken, in that the next two
  !!innocuous lines would crash this program.  The interp which fixes this may not
  !!be implemented until some time in 2007, if ever.  After that fix, we may want
  !!to include these lines and adjust the verification file.
  !
  ! dtarra = (/ dtarra /)
  ! dtarra = (/ derived:: dtarra /)

end program acetdt01k


! Extensions to introduce derived type parameters:
! type: derived - added parameters (kderived_1) to invoke with (4)/declare with (4) - 26 changes