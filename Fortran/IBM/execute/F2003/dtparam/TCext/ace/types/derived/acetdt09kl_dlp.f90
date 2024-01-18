! GM DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/ace/types/derived/acetdt09.f

!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetdt09
!*  TEST CASE NAME             : acetdt09kl_dlp
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt09
!*                               by David Forster)
!*  DATE                       : 2008-01-14 (original: 2006-08-09)
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : polymorphic, select type
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  AC's are can only be polymorphic if all ac_values are polymorphic and there
!*  is no type specifier.  Verify that a polymorphic AC can be used this way,
!*  and that the type is correct.  Here, we focus on derived types and
!*  inheritance.
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module mod

  type :: base(k1,l1)    ! (4,20)
      integer, kind :: k1
      integer, len  :: l1
   contains
     procedure :: printItem => basePrint
     generic :: write(formatted) => printItem
  end type base

  type, extends (base) :: derived    ! (4,20)
   contains
     procedure :: printItem => derivedPrint
     generic :: write(formatted) => printItem ! derivedPrint
  end type derived

contains

  subroutine basePrint(dtv,unit,iotype,vlist,iostat,iomsg)
    class(base(4,*)), intent(in) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg

    WRITE(unit, "('Base')", iostat=iostat)
  end subroutine basePrint

  subroutine derivedPrint(dtv,unit,iotype,vlist,iostat,iomsg)
    class(derived(4,*)), intent(in) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg

    WRITE(unit, "('Derived')", iostat=iostat)
  end subroutine derivedPrint

end module mod

program acetdt09kl_dlp

  use mod
  implicit none
  integer :: j, expType, actualType
  logical :: error

  type (derived(4,20)), target :: d
  type (base(4,20)), target    :: b

  class (base(4,:)), pointer    :: p, p2
  class (*), pointer       :: a(:)

  enum, bind(c)
  enumerator :: DTYPE, BTYPE, UTYPE
  end enum

  error = .false.

  do j=1,2

     p  => rainbow(j,expType)

     print *, 'For j=',j,', p=', (/ p /)

     select type ( o => (/ p /) )
     type is (derived(4,*));   actualType = DTYPE
     type is (base(4,*));      actualType = BTYPE
     class default;       actualType = UTYPE
     end select

     if (expType /= actualType) then
        print *, "Incorrect type: ", actualType, "should be", expType
        error = .true.
     end if

     ! Repeat for two-element AC

     p2 => rainbow(j,expType)
     print *, '...', (/ p, p2 /)
     select type ( o => (/ p, p2 /) )
     type is (derived(4,*));   actualType = DTYPE
     type is (base(4,*));      actualType = BTYPE
     class default;       actualType = UTYPE
     end select

     if (expType /= actualType) then
        print *, "Incorrect type (2): ", actualType, "should be", expType
        error = .true.
     end if

     ! Repeat with implied-do

     print *, '...', (/ (p, p2, j=1,2) /)
     select type ( o => (/ (p, p2, j=1,2) /) )
     type is (derived(4,*));   actualType = DTYPE
     type is (base(4,*));      actualType = BTYPE
     class default;       actualType = UTYPE
     end select

     if (expType /= actualType) then
        print *, "Incorrect type (3): ", actualType, "should be", expType
        error = .true.
     end if

     ! Repeat with implied-do (size zero)

     print *, '...', (/ (p, p2, j=1,0) /)
     select type ( o => (/ (p, p2, j=1,0) /) )
     type is (derived(4,*));   actualType = DTYPE
     type is (base(4,*));      actualType = BTYPE
     class default;       actualType = UTYPE
     end select

     if (expType /= actualType) then
        print *, "Incorrect type (4): ", actualType, "should be", expType
        error = .true.
     end if

  end do

  if (error) stop 2

contains
  function rainbow(sel,expType)
    integer, intent(in)  :: sel
    integer, intent(out) :: expType
    class(base(4,:)), pointer :: rainbow
    select case(sel)
    case(1); rainbow => d;  expType = DTYPE
    case(2); rainbow => b;  expType = BTYPE
    case default; rainbow => null(); print *, "Wrong selector value: ", sel
    end select
  end function rainbow

end program acetdt09kl_dlp
