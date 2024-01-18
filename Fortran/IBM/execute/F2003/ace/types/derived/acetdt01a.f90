!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-09-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : no-parameter derived TS component with defined assignment via elementals
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
!*  Like acetdt01, we test defined assignment, but here it is a component which
!*  has defined assignment.  Thus, we should see its use even if the structure
!*  of which it is part appears in an array constructor.  So, if assignment is
!*  user-defined for type S, and one of the components of type T is of type S,
!*  then we should see defined assignment being invoked in:
!*
!*      t1 = t(s(val))
!*      t2 = t1
!*      t1array = t1
!*      t2array = t1array
!*
!*  *as well as* within the array constructors below:
!*
!*      t1array = (/ t1 /)
!*      t2array = (/ t1array /)
!*
!*  Note that defined assignment is not invoked within a structure constructor
!*  or an array constructor, but will be invoked if a structure or array is
!*  assigned.
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
!*  assignment for arrays except by using either *elemental* type-bound procedures
!*  with passed-object dummy arguments (so no print statements, etc.), or
!*  nopass procedures with dummy array arguments and a generic interface.
!*
!*  Here we test type-bound procedures with passed-object dummy arguments,
!*  meaning we use elemental procedures, and cannot print trace output, but
!*  we can show that defined assignment has been used if we assign a different
!*  value than would be assigned by intrinsic assignment, e.g., by first doubling
!*  the value.  We test the values before and after to make sure that the test
!*  is valid, and that the correct form of assignment is used.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt01amod

  implicit none

  logical :: invalidDataEncountered = .false.

  type dt
     integer :: datum  = -1
   contains
     procedure :: dtAssignDt
     procedure :: dtAssignInt
     generic   :: assignment(=) => dtAssignDt, dtAssignInt
  end type dt

  type sContainer
     integer :: id = 0
     type (dt) :: d
  end type sContainer

contains

  elemental subroutine dtAssignDt(this, that)
    class (dt), intent(inout) :: this
    class (dt), intent(in) :: that
    this % datum = 2 * that % datum
  end subroutine dtAssignDt

  elemental subroutine dtAssignInt(this, that)
    class (dt), intent(inout) :: this
    integer, intent(in) :: that
    this % datum = 2 * that
  end subroutine dtAssignInt

  subroutine init(this, id, datum)
    class (sContainer), intent(inout) :: this
    integer, intent(in) :: id, datum
    this % id = id
    this % d % datum = datum
  end subroutine init

end module acetdt01amod


program acetdt01a

  use acetdt01amod
  implicit none
  type (sContainer) :: sc, sc2
  type (sContainer), target :: sca(2), sca2(2)
  type (sContainer), allocatable :: scall(:)
  type (sContainer), pointer :: scp(:)
  integer :: i

  call init(sc, 1, 101)
  call init(sc2, 2, 102)

  ! Set up arrays:
  call init(sca(1), 3,103)
  call init(sca(2), 4,104)
  call init(sca2(1), 5,105)
  call init(sca2(2), 6,106)

  !*      t1 = t(s(val))
  sc   = sContainer(7, dt(107))

  call prevalidate("sc2", sc2, sc)
  sc2  = sc
  call validateFromSource("sc2", sc2, sc)
  call prevalidateBoth("sca", sca, sc2, sc2)
  sca  = sc2
  call validateFromSources("sca", sca, sc2, sc2)

  ! Change the fields a little, so we can verify that datum is actually being assigned to:
  sca(1) % d % datum = 111
  sca(2) % d % datum = 112
  sc  % d % datum = 113
  sc2 % d % datum = 114
  call prevalidateBoth("sca2", sca2, sca(1), sca(2))
  sca2 = sca
  call validateFromSources("sca2", sca2, sca(1), sca(2))

  ! That was all preamble - making sure that things were initialised and that
  ! defined assignment happened when expected.  Now for the real tests:

  ! reset id fields so we can verify that id is actually being assigned to:
  sca(1) % id = 3;   sca(2) % id = 4;   sca2(1) % id = 5;   sca2(2) % id = 6

  call prevalidateBoth("sca  = (/ sc, sc2 /)",   sca, sc, sc2)
  sca  = (/ sc, sc2 /)
  call validateFromSources("sca  = (/ sc, sc2 /)",   sca, sc, sc2)
  call prevalidateBoth("sca2 = (/ sca /)",   sca2, sca(1), sca(2))
  sca2 = (/ sca /)
  call validateFromSources("sca2 = (/ sca /)",   sca2, sca(1), sca(2))

  ! reset id and datum fields so we can verify that they are actually being assigned to:
  sca(1) % id = 3;   sca(2) % id = 4;   sca2(1) % id = 5;   sca2(2) % id = 6
  sc  % d % datum = 115
  sc2 % d % datum = 116
  call prevalidateBoth("sca  = (/ sContainer:: sc, sc2 /)",   sca, sc, sc2)
  sca  = (/ sContainer:: sc, sc2 /)
  call validateFromSources("sca  = (/ sContainer:: sc, sc2 /)",   sca, sc, sc2)
  call prevalidateBoth("sca2 = (/ sContainer:: sca /)",   sca2, sca(1), sca(2))
  sca2 = (/ sContainer:: sca /)
  call validateFromSources("sca2 = (/ sContainer:: sca /)",   sca2, sca(1), sca(2))

  ! Repeat with implied-do:
  sca(1) % id = 3;   sca(2) % id = 4;   sca2(1) % id = 5;   sca2(2) % id = 6
  sca % d % datum = sca % d % datum - 1;   sca2 % d % datum = sca2 % d % datum - 1
  call prevalidateBoth("sca  = (/ sContainer:: (sc, sc2, i=1,1) /)",   sca, sc, sc2)
  sca  = (/ sContainer:: (sc, sc2, i=1,1) /)
  call validateFromSources("sca  = (/ sContainer:: (sc, sc2, i=1,1) /)",   sca, sc, sc2)
  call prevalidateBoth("sca2 = (/ sContainer:: (sca,i=1,1) /)",   sca2, sca(1), sca(2))
  sca2 = (/ sContainer:: (sca,i=1,1) /)
  call validateFromSources("sca2 = (/ sContainer:: (sca,i=1,1) /)",   sca2, sca(1), sca(2))

  allocate(scall(2))
  call init(scall(1), 11,121)
  call init(scall(2), 12,122)

  call prevalidateBoth("sca = (/ sContainer:: scall /)",   sca, scall(1), scall(2))
  sca = (/ sContainer:: scall /)
  call validateFromSources("sca = (/ sContainer:: scall /)",   sca, scall(1), scall(2))
  call prevalidateBoth("scall = (/ sContainer:: sca2 /)",   scall, sca2(1), sca2(2))
  scall = (/ sContainer:: sca2 /)
  call validateFromSources("scall = (/ sContainer:: sca2 /)",   scall, sca2(1), sca2(2))

  scp => sca
  call prevalidateBoth("scp = (/ sContainer:: scall /)",   scp, scall(1), scall(2))
  scp = (/ sContainer:: scall /)
  call validateFromSources("scp = (/ sContainer:: scall /)",   scp, scall(1), scall(2))

  ! Repeat with implied-do:
  sca(1) % id = 3;   sca(2) % id = 4;   sca2(1) % id = 5;   sca2(2) % id = 6
  sca % d % datum = sca % d % datum - 1;   sca2 % d % datum = sca2 % d % datum - 1
  call prevalidateBoth("sca = (/ sContainer:: (scall,i=1,1) /)",   sca, scall(1), scall(2))
  sca = (/ sContainer:: (scall,i=1,1) /)
  call validateFromSources("sca = (/ sContainer:: (scall,i=1,1) /)",   sca, scall(1), scall(2))
  call prevalidateBoth("scall = (/ sContainer:: (sca2,i=1,1) /)",   scall, sca2(1), sca2(2))
  scall = (/ sContainer:: (sca2,i=1,1) /)
  call validateFromSources("scall = (/ sContainer:: (sca2,i=1,1) /)",   scall, sca2(1), sca2(2))

  call prevalidateBoth("scp = (/ sContainer:: (scall,i=1,1) /)",   scp, scall(1), scall(2))
  scp = (/ sContainer:: (scall,i=1,1) /)
  call validateFromSources("scp = (/ sContainer:: (scall,i=1,1) /)",   scp, scall(1), scall(2))

  if (invalidDataEncountered) stop 2

contains

  ! These routines warn if data cannot be used to show that assignment is happening correctly:
  subroutine prevalidateBoth(text, tgt, src1, src2)
    character(*), intent(in) :: text
    class (sContainer), intent(in) :: tgt(2), src1, src2
    call prevalidate(text // "<1>", tgt(1), src1)
    call prevalidate(text // "<2>", tgt(2), src2)
  end subroutine prevalidateBoth

  subroutine prevalidate(text, tgt, src)
    character(*), intent(in) :: text
    class (sContainer), intent(in) :: tgt, src
    integer :: eventualID, eventualDatum
    eventualID = src % id
    eventualDatum = 2 * src % d % datum
    if (tgt % id == eventualID) print *, "At ", text, " no change in ID: ", tgt % id, eventualID
    if (tgt % d % datum == eventualDatum) print *, "At ", text, " no change in datum: ", tgt % d % datum, eventualDatum
!!$    ! Uncomment this block if you need to see what data is being used in the test:
!!$    if (allocated(scall)) then
!!$       if (associated(scp)) then
!!$          print *, "c", sc, "c2", sc2, "ca", sca, "ca2", sca2, "call", scall, "cp", scp
!!$       else
!!$          print *, "c", sc, "c2", sc2, "ca", sca, "ca2", sca2, "call", scall
!!$       endif
!!$    else
!!$       print *, "c", sc, "c2", sc2, "ca", sca, "ca2", sca2
!!$    endif
  end subroutine prevalidate

  ! These routines verify that the correct form of assignment has been used:
  subroutine validateFromSources(text, tgt, src1, src2)
    character(*), intent(in) :: text
    class (sContainer), intent(in) :: tgt(2), src1, src2
    call validateFromSource(text, tgt(1), src1)
    call validateFromSource(text, tgt(2), src2)
  end subroutine validateFromSources

  subroutine validateFromSource(text, tgt, src)
    character(*), intent(in) :: text
    class (sContainer), intent(in) :: tgt, src
    integer :: expectedID, expectedDatum
    expectedID = src % id
    expectedDatum = 2 * src % d % datum
    if (tgt % id /= expectedID .or. tgt % d % datum /= expectedDatum) then
       print *, "Unexpected results at ", text, ":", tgt % id, ",", tgt % d % datum, "should be", expectedID, ",", expectedDatum
       invalidDataEncountered = .true.
    endif
  end subroutine validateFromSource

end program acetdt01a
