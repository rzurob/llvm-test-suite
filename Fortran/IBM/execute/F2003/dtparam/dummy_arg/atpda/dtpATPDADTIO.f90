!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpATPDADTIO
!*
!*  DATE                       : 2008-10-15
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : user-defined formatted I/O
!*
!*  REFERENCE                  : Feature Number 357495
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Perform I/O via user-defined procedures.  Assumed-type parameters are present
!*  in both the I/O routines and some of the invoking routines.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpATPDADTIOMod

  implicit none

  type Student(snBytes,longestFirstName,longestLastName)
     integer, len :: longestFirstName, longestLastName
     integer, kind :: snBytes
     character(longestFirstName + longestLastName + 1) :: name
     integer(snBytes) :: studentNumber
   contains
     procedure, pass :: printStudent4
     procedure, pass :: printStudent2
     generic :: print => printStudent4, printStudent2
     procedure, pass :: writeStudent4
     procedure, pass :: readStudent4
     procedure, pass :: writeStudent2
     procedure, pass :: readStudent2
     generic :: write(formatted) => writeStudent4, writeStudent2
     generic :: read (formatted) => readStudent4,  readStudent2
  end type Student

contains

  subroutine readStudent4 (this, unit, iotype, vlist, iostat, iomsg)
    class(Student(4,*,*)), intent(inout) :: this
    integer, intent(in) :: unit
    character (*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (*), intent(inout) :: iomsg
    character (50) :: pfmt
    integer :: k1, l1, l2
    write(pfmt,"(a,i2,a,i1,a)") "('Student(',i2,',',i2,',',i2,')',a", vlist(1), ",i", vlist(2), ")"
    read(unit, fmt=pfmt, iostat=iostat, iomsg=iomsg) k1, l1, l2, this%name, this%studentNumber
    if (iostat == 0) then
       if (k1 /= this%snBytes) iostat = 1
       if (l1 /= this%longestFirstName) iostat = iostat + 2
       if (l2 /= this%longestLastName) iostat = iostat + 4
       if (iostat /= 0) iomsg = 'Parameter error (Student)'
    end if
  end subroutine readStudent4

  subroutine writeStudent4 (this, unit, iotype, vlist, iostat, iomsg)
    class(Student(4,*,*)), intent(in) :: this
    integer, intent(in) :: unit
    character (*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (*), intent(inout) :: iomsg
    character (50) :: pfmt
    write(pfmt,"(a,i2,a,i1,a)") "('Student(',i2,',',i2,',',i2,')',a", vlist(1), ",i", vlist(2), ")"
    write(unit,fmt=pfmt,iostat=iostat,iomsg=iomsg) this % snBytes, this%longestFirstName, this%longestLastName, this%name, this%studentNumber
  end subroutine writeStudent4

  subroutine printStudent4(this)
    class (Student(4,*,*)) :: this
    print *, this%name, ":", this%studentNumber
  end subroutine printStudent4


  subroutine readStudent2 (this, unit, iotype, vlist, iostat, iomsg)
    class(Student(2,*,*)), intent(inout) :: this
    integer, intent(in) :: unit
    character (*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (*), intent(inout) :: iomsg
    character (50) :: pfmt
    integer :: k1, l1, l2
    write(pfmt,"(a,i2,a,i1,a)") "('Student(',i2,',',i2,',',i2,')',a", vlist(1), ",i", vlist(2), ")"
    read(unit, fmt=pfmt, iostat=iostat, iomsg=iomsg) k1, l1, l2, this%name, this%studentNumber
    if (iostat == 0) then
       if (k1 /= this%snBytes) iostat = 1
       if (l1 /= this%longestFirstName) iostat = iostat + 2
       if (l2 /= this%longestLastName) iostat = iostat + 4
       if (iostat /= 0) iomsg = 'Parameter error (Student)'
    end if
  end subroutine readStudent2

  subroutine writeStudent2 (this, unit, iotype, vlist, iostat, iomsg)
    class(Student(2,*,*)), intent(in) :: this
    integer, intent(in) :: unit
    character (*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (*), intent(inout) :: iomsg
    character (50) :: pfmt
    write(pfmt,"(a,i2,a,i1,a)") "('Student(',i2,',',i2,',',i2,')',a", vlist(1), ",i", vlist(2), ")"
    write(unit,fmt=pfmt,iostat=iostat,iomsg=iomsg) this % snBytes, this%longestFirstName, this%longestLastName, this%name, this%studentNumber
  end subroutine writeStudent2

  subroutine printStudent2(this)
    class (Student(2,*,*)) :: this
    print *, this%name, ":", this%studentNumber
  end subroutine printStudent2


  subroutine readOne(unit, fmt, s2, s4, which)
    class (Student(2,*,*)), intent(inout) :: s2
    class (Student(4,*,*)), intent(inout) :: s4
    integer :: unit, which, istat
    character(*) :: fmt
    character(255) :: msg
    istat = 0
    if (which == 2) then
       read(unit,fmt=fmt,iostat=istat,iomsg=msg) s2
    else
       read(unit,fmt=fmt,iostat=istat,iomsg=msg) s4
    end if
    if (istat /= 0) print *, "Error",istat,"in readOne: ", msg
  end subroutine readOne


  subroutine writeOne(unit, fmt, s2, s4, which)
    class (Student(2,*,*)) :: s2
    class (Student(4,*,*)) :: s4
    integer :: unit, which, istat
    character(*) :: fmt
    character(255) :: msg
    istat = 0
    if (which == 2) then
       write(unit,fmt=fmt,iostat=istat,iomsg=msg) s2
    else
       write(unit,fmt=fmt,iostat=istat,iomsg=msg) s4
    end if
    if (istat /= 0) print *, "Error",istat,"in writeOne: ", msg
  end subroutine writeOne

end module dtpATPDADTIOMod

program dtpATPDADTIO

  use :: dtpATPDADTIOMod
  implicit none
  type(Student(4,6,8))  :: s4_1, s4_2, s4_1a, s4_2a, s4_1b, s4_2b
  type(Student(2,9,3))  :: s2_1, s2_2, s2_1a, s2_2a, s2_1b, s2_2b
  character(255) :: msg
  integer :: istat

  s4_1 = Student(4,6,8) ("abcdefgHIJKLMNO",1234567)
  s4_2 = Student(4,6,8) ("ABCDEFGhijklmno",4567890)

  s2_1 = Student(2,9,3) ("pqrstuvwx_XYZ",32000)
  s2_2 = Student(2,9,3) ("PQRSTUVWX_xyz",-22000)

  print *, "Original data:"
  call s4_1 % print
  call s4_2 % print

  call s2_1 % print
  call s2_2 % print

  ! write the data
  open(unit=3,file="dtpATPDADTIO.tmp", action="WRITE") ! default of sequential
  write(3, fmt="(DT(20,9))", iostat=istat, iomsg=msg) s4_1, s4_2
  if (istat /= 0) then
     print *, istat, msg
     stop 2
  end if
  write(3, fmt="(DT(13,6))", iostat=istat, iomsg=msg) s2_1, s2_2
  if (istat /= 0) then
     print *, istat, msg
     stop 3
  end if
  call writeOne(3,"(DT(13,6))", s2_1, s4_1, 2)
  call writeOne(3,"(DT(17,9))", s2_1, s4_1, 4)
  call writeOne(3,"(DT(13,6))", s2_2, s4_2, 2)
  call writeOne(3,"(DT(17,8))", s2_2, s4_2, 4)
  close(unit=3)

  ! read it back
  open(unit=4,file="dtpATPDADTIO.tmp", action="READ") ! default of sequential
  read(4, fmt="(DT(20,9))", iostat=istat, iomsg=msg) s4_1a, s4_2a
  if (istat /= 0) then
     print *, istat, msg
     stop 4
  end if
  read(4, fmt="(DT(13,6))", iostat=istat, iomsg=msg) s2_1a, s2_2a
  if (istat /= 0) then
     print *, istat, msg
     stop 5
  end if

  print *, "Read-in data:"
  call s4_1a % print
  call s4_2a % print
  call s2_1a % print
  call s2_2a % print

  ! and verify it
  if (s4_1%name /= s4_1a%name .or. s4_1%studentNumber /= s4_1a%studentNumber) stop 10
  if (s4_2%name /= s4_2a%name .or. s4_2%studentNumber /= s4_2a%studentNumber) stop 11
  if (s2_1%name /= s2_1a%name .or. s2_1%studentNumber /= s2_1a%studentNumber) stop 12
  if (s2_2%name /= s2_2a%name .or. s2_2%studentNumber /= s2_2a%studentNumber) stop 13

  call readOne(4,"(DT(13,6))", s2_1b, s4_1b, 2)
  call readOne(4,"(DT(17,9))", s2_1b, s4_1b, 4)
  call readOne(4,"(DT(13,6))", s2_2b, s4_2b, 2)
  call readOne(4,"(DT(17,8))", s2_2b, s4_2b, 4)

  print *, "Read-in data(2):"
  call s4_1b % print
  call s4_2b % print
  call s2_1b % print
  call s2_2b % print

  if (s4_1%name /= s4_1b%name .or. s4_1%studentNumber /= s4_1b%studentNumber) stop 20
  if (s4_2%name /= s4_2b%name .or. s4_2%studentNumber /= s4_2b%studentNumber) stop 21
  if (s2_1%name /= s2_1b%name .or. s2_1%studentNumber /= s2_1b%studentNumber) stop 22
  if (s2_2%name /= s2_2b%name .or. s2_2%studentNumber /= s2_2b%studentNumber) stop 23

  close(unit=4)

  print *, 'done'

end program dtpATPDADTIO
