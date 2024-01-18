! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : position101kl
!*
!*  PROGRAMMER                 : David Forster (derived from position101 by Robert Ma)
!*  DATE                       : 2007-07-06 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try position edit descriptors (T, TL, TR, X) inside DTIO (Input)
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

   type base (kb,lb)
      integer, kind :: kb
      integer, len :: lb
      integer(kb)   :: id
      character(lb) :: name = ''
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program position101kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4,:)), allocatable :: b1 ! tcx: (4,:)
   class(base(4,:)), pointer     :: b2 ! tcx: (4,:)
   type (base(4,3))              :: b3 ! tcx: (4,3)

   namelist /nml/ b1, b2, b3

   allocate( b1, source = base(4,3)(101,'ibm')) ! tcx: (4,3)
   allocate( b2, source = base(4,3)(202,'ftn')) ! tcx: (4,3)
   b3 = base(4,3) ( 303, 'FTN') ! tcx: (4,3)

   open (1, file = 'position101kl.1', form='formatted', access='sequential' )

   read (1,NML=nml, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   
   if ( ( b1%name /= 'abc' ) .or. ( b1%id /= 101 ) ) error stop 2_4
   if ( ( b2%name /= 'def' ) .or. ( b2%id /= 102 ) ) error stop 3_4
   if ( ( b3%name /= 'ghi' ) .or. ( b3%id /= 103 ) ) error stop 4_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 5_4
   if ( size(v_list, 1) /= 0 ) error stop 6_4

   read ( unit, "(T9,I4,TL8,A3,/)", iostat = iostat )      dtv%id, dtv%name
   ! read one more time in reverse order
   read ( unit, "(T1, I4, TR1, 1X, A3)", iostat = iostat ) dtv%id, dtv%name

   iomsg = 'dtioread'

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 8 changes
