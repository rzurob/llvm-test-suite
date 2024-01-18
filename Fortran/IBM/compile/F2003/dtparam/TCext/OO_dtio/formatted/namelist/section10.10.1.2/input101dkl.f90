!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : input101dkl
!*
!*  PROGRAMMER                 : David Forster (derived from input101d by Robert Ma)
!*  DATE                       : 2007-07-20 (original: 21/03/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: derived type containing allocatable component requires DTIO (do not provide DTIO)
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
   type data (kd) ! kd=4
      integer, kind :: kd
      integer(kd)   :: i
   end type

   type base (kb) ! kb=4
      integer, kind :: kb
      class(data(kb)), allocatable  :: d2 ! tcx: (kb)
   end type

end module

program input101dkl
   use m

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         class(data(4)), intent(inout) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat = -99
   character(150) :: msg = ''

   type(base(4)) :: b1 ! tcx: (4)
   namelist /nml/ b1

   allocate(b1%d2)

   open (1, file='input101dkl.1', form='formatted', access='sequential', blank='zero' )

   read (1, nml, iostat = stat, iomsg = msg)

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: data

   class(data(4)), intent(inout) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   type(data(4)) :: d1 ! tcx: (4)
   namelist /nml/ d1

   if ( iotype /= 'NAMELIST' ) error stop 3_4
   if ( size(v_list,1) /= 0 )  error stop 4_4
   
   read (unit, nml, iostat=iostat )
    
   dtv%i = d1%i
   iomsg = 'dtioread'

end subroutine



! Extensions to introduce derived type parameters:
! type: data - added parameters (kd) to invoke with (4) / declare with (4) - 4 changes
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 1 changes
