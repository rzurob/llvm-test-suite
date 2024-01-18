! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : input103akl
!*
!*  PROGRAMMER                 : David Forster (derived from input103a by Robert Ma)
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
!*                                       (no dtio procedure involved, with polymorphic component)
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

   type innerdata (lid) ! lid=3
      integer, len :: lid
      character(lid) :: c   = 'xxx'
   end type
   
   type data (kd,ld) ! kd,ld=4,3
      integer, kind :: kd
      integer, len :: ld
      integer(kd)   :: i = -999
      type(innerdata(ld)) :: i1 ! tcx: (ld)
   end type

   type base (kb,lb) ! kb,lb=4,3
      integer, kind :: kb
      integer, len :: lb
      integer(kb)   :: j = -99
      type(data(kb,lb))   :: d1 ! tcx: (kb,lb)
      ! expanded into this order : base%j, base%d1%i, base%d1%i1%c
   end type

end module

program input103akl
   use m

   integer :: stat
   character(150) :: msg

   type(base(4,3)) :: b1 ! tcx: (4,3)
   type(base(4,:)), allocatable :: b2 ! tcx: (4,:)
   namelist /nml/ b1, b2
   allocate(base(4,3):: b2) ! tcx: base(4,3)

   open (1, file='input103akl.1', form='formatted', access='sequential' )
   
   read (1, nml, iostat = stat, iomsg = msg)

   if ( ( b1%j /= 101 ) .or. ( b1%d1%i /= 1001 ) .or. ( b1%d1%i1%c /= 'abc' ) )  error stop 1_4
   if ( ( b2%j /= 202 ) .or. ( b2%d1%i /= 2002 ) .or. ( b2%d1%i1%c /= 'def' ) )  error stop 2_4

end program


! Extensions to introduce derived type parameters:
! type: innerdata - added parameters (lid) to invoke with (3) / declare with (*) - 1 changes
! type: data - added parameters (kd,ld) to invoke with (4,3) / declare with (4,*) - 1 changes
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 2 changes
