! GM DTP extension using:
! ftcx_dtp -qnok -qnol -qdefaultpv /tstdev/F2003/ace/types/derived/acetdt57.f

!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-23 (original: 2006-11-23)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : derived type, component, default
!*                               initialisation, default initialization
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Create some derived type instances which refer to initialisations involving
!*  other derived types.  Reference to other objects of the same type IS tricky:
!*  you can only use pointers, meaning that a default initialisation is not
!*  really possible, excepting null. Self-reference is the same: we can only do
!*  this in executable code.
!*
!*  Changes:
!*  20070219 dforster attempt to get size of array pointee which may not have
!*  been properly set up; solution: check for ASSOCIATED before checking the
!*  size.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt57_dpvmod

  implicit none
  integer :: i

  type Contained(k1)    ! (4)
     integer, kind :: k1
     integer(k1)   :: ival
  end type Contained

  type DerivedEx
     type (Contained(4)) :: cval(3) = [Contained(4):: Contained(4)(1), Contained(4)(2), Contained(4)(3)]
  end type DerivedEx

  type DerivedImp
     type (Contained(4)) :: cval(3) = [Contained(4):: (Contained(4)(i), i=1,3)]
  end type DerivedImp

  type DerivedSelf
     type (Contained(4)) :: cval(3) = [Contained(4):: Contained(4)(1), Contained(4)(2), Contained(4)(3)]
     type (DerivedSelf), pointer :: dsval(:) => null()
   contains
     procedure :: printItem => dsPrint
     generic :: write(formatted) => printItem
  end type DerivedSelf

contains

  subroutine dsPrint(this,unit,iotype,vlist,iostat,iomsg)
    class(DerivedSelf), intent(in) :: this
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    character (100) :: tmp, subtree
    integer :: i

    subtree = ''
    tmp = ''
    ! We're going to be lazy - just go one level deep...
    if (associated(this % dsval)) then
       write(subtree,"(i1,':')") size(this % dsval)
       do i=1,size(this % dsval)
          write(tmp,*) this % dsval(i) % cval
          subtree = trim(subtree) // "," // trim(tmp) // merge('*','-',associated(this % dsval(i) % dsval) .and. size(this % dsval(i) % dsval) > 0)
       end do
    end if

    write(tmp,*) this % cval
    write(unit, "('DS[',a,',',a,']')", iostat=iostat) trim(tmp), trim(subtree)
  end subroutine dsPrint

end module acetdt57_dpvmod

program acetdt57_dpv

  use acetdt57_dpvmod
  implicit none
  type (DerivedEx) :: de
  type (DerivedEx), allocatable :: dea

  type (DerivedImp) :: di
  type (DerivedImp), allocatable :: dia

  type (DerivedSelf) :: ds
  type (DerivedSelf), target  :: dsarr0(0) = [DerivedSelf:: ]
  type (DerivedSelf), target  :: dsarr1(1) = [DerivedSelf:: DerivedSelf([Contained(4):: (Contained(4)(i),i=13,15)])]
  type (DerivedSelf), target  :: dsarr2(2) = [DerivedSelf:: DerivedSelf([Contained(4):: (Contained(4)(i),i=9,7,-1)]), &
                                              DerivedSelf([Contained(4):: (Contained(4)(i),i=22,24)])]
  type (DerivedSelf), allocatable :: dsa

  print *, de
  allocate(dea)
  print *, dea

  print *, di
  allocate(dia)
  print *, dia

  dsarr2(1) % dsval => dsarr0
  dsarr2(2) % dsval => dsarr1
  ds        % dsval => dsarr0
  print *, ds
  ds % dsval => dsarr1
  print *, ds

  allocate(dsa)
  dsa % dsval => dsarr2
  print *, dsa

  dsarr2(2) % dsval => dsarr2
  print *, dsa

end program acetdt57_dpv
