! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-06 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with data object with protected attribute (output)
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
   type base (kb)
      integer, kind :: kb
      complex(kb) :: c
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4)), pointer, protected :: b1 ! tcx: (4)
   class(base(4)), allocatable, protected :: b2 ! tcx: (4)
   type(base(4)), pointer, protected :: b3 ! tcx: (4)
   type(base(4)), allocatable, protected :: b4 ! tcx: (4)

   namelist /b1b3/ b1, b3

   contains

   subroutine assoMe (c)
      complex(4), intent(in), optional :: c

      if ( present(c) ) then
         allocate ( b1, source = base(4)(c) ) ! tcx: (4)
         allocate ( b3, source = base(4)(c) ) ! tcx: (4)
      else
      	 allocate ( b1, source = base(4)( (1.0,1.0) ) ) ! tcx: (4)
      	 allocate ( b3, source = base(4)(c) ) ! tcx: (4)
      end if

   end subroutine

   subroutine allocMe (c)
      complex(4), intent(in), optional :: c

      if ( present(c) ) then
         allocate ( b2, source = base(4)(c) ) ! tcx: (4)
         allocate ( b4, source = base(4)(c) ) ! tcx: (4)
      else
      	 allocate ( b2, source = base(4)( (1.0,3.0) ) ) ! tcx: (4)
      	 allocate ( b4, source = base(4)( (2.0,4.0) ) ) ! tcx: (4)
      end if

   end subroutine

end module

program protected001kl
use m
   namelist /b2b4/ b2, b4
   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'protected001kl.1', form='formatted', access='sequential' )

   call assoMe((2.0,3.0))
   call allocMe()

   write (1, b1b3, iostat = stat, iomsg = msg )
   write (1, b2b4, iostat = stat, iomsg = msg )

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(4)), intent(in) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 1_4
   if ( size(v_list, 1) /= 0 ) error stop 2_4

   write (unit, "('c= (',F6.3,',',F6.3,')')", iostat=iostat )          dtv%c

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 14 changes