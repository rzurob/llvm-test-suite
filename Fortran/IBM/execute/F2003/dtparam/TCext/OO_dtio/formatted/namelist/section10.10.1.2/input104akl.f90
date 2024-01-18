! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : input104akl
!*
!*  PROGRAMMER                 : David Forster (derived from input104a by Robert Ma)
!*  DATE                       : 2007-07-20 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.1 Namelist Input Values
!*                                        Derived type variable shall be expanded into intrinsic types
!*                                       (no dtio procedure involved, with array components)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type base (kb,lb) ! kb,lb=4,3
      integer, kind :: kb
      integer, len :: lb
      integer(kb)   :: i(lb) = 9 ! (/ 9, 9, 9 /)
   end type

   type, extends(base) :: child (kc,lc) ! kc,lc=4,3
      integer, kind :: kc
      integer, len :: lc
      real(kc)      :: r(lc) = 9.0 ! (/ 9.0, 9.0, 9.0 /)
   end type

   type, extends(child) :: gen3 (lg1,lg2) ! lg1,lg2=3,3
      integer, len :: lg1,lg2
      character(lg1) :: c(lg2) = 'xxx' ! (/ 'xxx', 'xxx', 'xxx' /)
   end type
end module

program input104akl
   use m

   integer :: stat
   character(150) :: msg = ''
   procedure(logical) :: precision_r4
   type(child(4,3,4,3))               :: b1 ! tcx: (4,3,4,3)
   type(child(4,:,4,:)), pointer      :: b2 ! tcx: (4,:,4,:)
   type(gen3(4,3,4,3,3,3))                :: b3 ! tcx: (4,3,4,3,3,3)
   type(gen3(4,:,4,:,:,:)), allocatable   :: b4 ! tcx: (4,:,4,:,:,:)

   namelist /n1/ b1, b2
   namelist /n1/ b3, b4

   allocate(child(4,3,4,3):: b2) ! tcx: child(4,3,4,3)
   allocate(gen3(4,3,4,3,3,3):: b4) ! tcx: gen3(4,3,4,3,3,3)

   open (1, file='input104akl.1', form='formatted', access='sequential', blank='zero' )

   read (1, n1, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= '' ) ) error stop 1_4

   print *, b1
   print *, b2
   print *, b3
   print *, b4

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 0 changes
! type: child - added parameters (kc,lc) to invoke with (4,3,4,3) / declare with (4,*,4,*) - 2 changes
