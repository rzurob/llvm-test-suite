! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : position002kl
!*
!*  PROGRAMMER                 : David Forster (derived from position002 by Robert Ma)
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
!*                                        Try position edit descriptors (T, TL, TR, X) with multiple level of namelist DTIO(Output)
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
   type data (kd)
      integer, kind :: kd
      integer(kd) :: k
   end type

   type container (kc1,kc2)
      integer, kind :: kc1,kc2
      integer(kc1) :: j
      type(data(kc2)), pointer :: d ! tcx: (kc2)
   end type

   type base (kb1,kb2,kb3)
      integer, kind :: kb1,kb2,kb3
      integer(kb1) :: i
      type(container(kb2,kb3)), pointer :: c ! tcx: (kb2,kb3)
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4,4,4)), intent(in) :: dtv ! tcx: (4,4,4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program position002kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4,4,4)), allocatable :: b1 ! tcx: (4,4,4)
   class(base(4,4,4)), pointer     :: b2 ! tcx: (4,4,4)
   namelist /nml/ b1, b2

   allocate( b1, b1%c, b1%c%d )
   allocate( b2, b2%c, b2%c%d )
   
   b1%i = 1
   b1%c%j= 2
   b1%c%d%k= 3
   b2%i = 11
   b2%c%j= 12
   b2%c%d%k= 13

   open (1, file = 'position002kl.1', form='formatted', access='sequential' )

   write (1,NML=nml, iostat=stat, iomsg=msg)

   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, container
   interface write(formatted)
      subroutine containerwriteformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import container
         class(container(4,4)), intent(in) :: dtv ! tcx: (4,4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4,4,4)), intent(in) :: dtv ! tcx: (4,4,4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   type(container(4,4)), allocatable :: c1 ! tcx: (4,4)

   namelist /basedtio/ c1

   allocate ( c1, source = dtv%c )

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   write (unit, "(2X,I4, TL10,'i=',6x)", iostat=iostat)  dtv%i

   if ( iostat /= 0 ) error stop 4_4

   write (unit, basedtio, iostat=iostat, iomsg=iomsg)
   
   if ( ( iostat /= 0 ) .or. ( iomsg /= 'containerdtio' )) error stop 5_4

   iomsg = 'dtiowrite'

end subroutine

subroutine containerwriteformatted(dtv, unit, iotype, v_list, iostat, iomsg )
   use m, only: container, data
   
   interface write(formatted)
      subroutine datawriteformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         class(data(4)), intent(in) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(container(4,4)), intent(in) :: dtv ! tcx: (4,4)
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   type(data(4)), allocatable :: d1 ! tcx: (4)
   namelist /containerdtio/ d1

   allocate (d1, source = dtv%d)
   
   if ( iotype /= "NAMELIST" ) error stop 6_4
   if ( size(v_list, 1) /= 0 ) error stop 7_4

   write (unit, "('i=',I4)", iostat=iostat)  dtv%j

   if ( iostat /= 0 ) error stop 8_4

   write (unit, containerdtio, iostat=iostat, iomsg=iomsg)
   if ( ( iostat /= 0 ) .or. ( iomsg /= 'datadtio' )) error stop 9_4
   
   iomsg = 'containerdtio'

end subroutine

subroutine datawriteformatted(dtv, unit, iotype, v_list, iostat, iomsg )
   use m, only: data

   class(data(4)), intent(in) :: dtv ! tcx: (4)
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg
   
   if ( iotype /= "NAMELIST" ) error stop 10_4
   if ( size(v_list, 1) /= 0 ) error stop 11_4

   write (unit, "(T1,'i=',T1,TR2,I4)", iostat=iostat )  dtv%k

   iomsg = 'datadtio'

end subroutine


! Extensions to introduce derived type parameters:
! type: data - added parameters (kd) to invoke with (4) / declare with (4) - 4 changes
! type: container - added parameters (kc1,kc2) to invoke with (4,4) / declare with (4,4) - 4 changes
! type: base - added parameters (kb1,kb2,kb3) to invoke with (4,4,4) / declare with (4,4,4) - 4 changes
