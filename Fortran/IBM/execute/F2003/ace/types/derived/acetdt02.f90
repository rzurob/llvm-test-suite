!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-09-08
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : no-parameter derived TS with defined assignment via interface construct
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : derived type, defined assignment, interface
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
!*  There are three ways in which defined assignment can be set up:
!*
!*  1. by use of a generic binding to assignment(=) in the type definition,
!*     using type-bound procedures and passed-object dummy arguments, or
!*  2. by use of a generic interface and type-bound nopass procedures, or
!*  3. by use of a generic interface and procedures which are not type-bound.
!*
!*  C461 requires specific bindings for assignment(=) to have a passed-object
!*  dummy argument, which must be "a scalar, nonpointer, nonallocatable dummy
!*  data object", according to C453.  This means that we cannot define
!*  assignment for arrays except by using either *elemental* type-bound
!*  procedures with passed-object dummy arguments (so no print statements,
!*  etc.), or nopass procedures with dummy array arguments and a generic
!*  interface.
!*
!*  Here we test the second way: a generic interface and type-bound nopass
!*  procedures with various dummy arguments (since they're not elemental, we can
!*  print every time we assign, and see clearly that things are working
!*  correctly).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt02mod

  implicit none

  integer :: timeMark

  type dt
     integer :: datum  = -1
     integer :: tag    = -1
     integer :: singleAssignWasInvoked = 0
     integer :: arrayAssignWasInvoked  = 0
     integer :: array2AssignWasInvoked = 0
   contains
     procedure, nopass :: dtAssignSingle
     procedure, nopass :: dtAssignArray
     procedure, nopass :: dtAssignArray2
     procedure, nopass :: dtToString
     procedure, nopass :: dtAToString
  end type dt

  interface assignment(=)
     module procedure dtAssignArray
     module procedure dtAssignSingle
     module procedure dtAssignArray2
  end interface

contains

  subroutine dtAssignSingle(this, that)
    class (dt), intent(inout) :: this
    class (dt), intent(in) :: that
    character(1000) :: thisBefore, thatBefore
    timeMark = timeMark + 1
    thatBefore = trim(dtToString(that))
    thisBefore = trim(dtToString(this))
    this % singleAssignWasInvoked = this % singleAssignWasInvoked + 1
    this % datum = that % datum
    print *, "dtS: Assigned ", trim(thatBefore), " to ", trim(thisBefore), " yielding ", trim(dtToString(this)), " at", timeMark
  end subroutine dtAssignSingle

  subroutine dtAssignArray(this, that)
    class (dt), intent(inout) :: this(:)
    class (dt), intent(in) :: that
    character(1000) :: thisBefore, thatBefore
    timeMark = timeMark + 1
    thatBefore = trim(dtToString(that))
    thisBefore = trim(dtAToString(this))
    this % arrayAssignWasInvoked = this % arrayAssignWasInvoked + 1
    this % datum = that % datum
    print *, "dtA: Assigned ", trim(thatBefore), " to ", trim(thisBefore), " yielding ", trim(dtAToString(this)), " at", timeMark
  end subroutine dtAssignArray

  subroutine dtAssignArray2(this, that)
    class (dt), intent(inout) :: this(:)
    class (dt), intent(in) :: that(:)
    character(1000) :: thisBefore, thatBefore
    timeMark = timeMark + 1
    thatBefore = trim(dtAToString(that))
    thisBefore = trim(dtAToString(this))
    this % array2AssignWasInvoked = this % array2AssignWasInvoked + 1
    this % datum = that % datum
    print *, "dtA2: Assigned ", trim(thatBefore), " to ", trim(thisBefore), " yielding ", trim(dtAToString(this)), " at", timeMark
  end subroutine dtAssignArray2

  function dtToString(this)
    class (dt), intent(in) :: this
    character(100) :: dtToString
    write(dtToString,'("dt[t=",i0,",d=",i0,",s=",i0,",a=",i0,",a2=",i0,"]")') this % tag, this % datum, &
         & this % singleAssignWasInvoked, this % arrayAssignWasInvoked, this % array2AssignWasInvoked
  end function dtToString

  function dtAToString(this)
    class (dt), intent(in) :: this(:)
    character(100*size(this)) :: tmpString
    character(100*size(this)) :: dtAToString
    integer :: i
    write(tmpString,'(100(i0,":",a:","))') (i, trim(dtToString(this(i))), i = 1,size(this))
    write(dtAToString,'("dt{",a,"}")') trim(tmpString)
  end function dtAToString

end module acetdt02mod


program acetdt02

  use acetdt02mod
  implicit none
  type (dt) :: dt_1, dt_2
  type (dt) :: dt_array_1(2), dt_array_2(2)

  type (dt), target :: dt_t
  type (dt), pointer :: dt_p
  type (dt), allocatable :: dt_a

  print *, "Start"
  dt_1 % tag = 1
  dt_2 % tag = 2
  dt_array_1 % tag = (/ 3, 4 /)
  dt_array_2 % tag = (/ 5, 6 /)

  print *, "";  print *, "Block 1:"
  dt_2 = dt(30,7)		! should invoke single->single defined assignment
  dt_1 = dt_2			! should invoke single->single defined assignment
  dt_array_2 = dt_2		! should invoke single->array defined assignment
  dt_array_1 = dt_array_2	! should invoke array->array defined assignment
  dt_array_2(2) = dt_array_1(1)	! should invoke single->single defined assignment

  print *, "";  print *, "Block 2:"
  dt_array_2 = (/ dt_1, dt_2 /)	! should invoke array->array defined assignment once, and not single->single or single->array
  dt_array_1 = (/ dt_array_2 /)	! should invoke array->array defined assignment once only, and not single->single or single->array
  dt_array_2 = (/ dt:: dt_1, dt_2 /)	! should invoke array->array defined assignment once, and not single->single or single->array
  dt_array_1 = (/ dt:: dt_array_2 /)	! should invoke array->array defined assignment once only, and not single->single or single->array

  print *, "";  print *, "Block 3:"
  ! none of the following should invoke any defined assignment
  dt_t % tag = 8
  dt_p => dt_t
  allocate(dt_a)
  dt_a % tag = 9

  print *, "";  print *, "Block 4:"
  ! each of the following should invoke only single->single defined assignment
  dt_t = dt(31,10)
  dt_p = dt(32,11)
  dt_a = dt(33,12)

  print *, "";  print *, "Block 5:"
  ! each of the following should invoke only array->array defined assignment - each once only
  dt_array_1 = (/ dt(34,13), dt_p /)
  dt_array_2 = (/ dt(tag=14,datum=35), dt_t /)
  dt_array_1 = (/ dt_t, dt_a /)
  dt_array_1 = (/ dt_array_1 /)
  dt_array_2 = (/ dt:: dt(36,15), dt_t /)
  dt_array_1 = (/ dt:: dt_p, dt(tag=16,datum=37) /)
  dt_array_2 = (/ dt:: dt(38,17), dt_a /)
  dt_array_2 = (/ dt:: dt_array_2 /)

  print *, "";  print *, "End"

end program acetdt02
