! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : access001akl
!*
!*  DATE                       : 2007-06-20 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with namelist and object of public/private accessibility (output)
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
      integer(kb), private :: i
      contains
      procedure, pass :: geti
      procedure, pass :: seti
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4)), allocatable :: b1
   class(base(4)), pointer     :: b2
   type(base(4))               :: b3
   integer :: stat
   character(150) :: msg
   namelist /n123/ b1, b2, b3

   contains

   subroutine seti(dtv, i)
      class(base(4)), intent(inout) :: dtv
      dtv%i = i
   end subroutine

   integer function geti(dtv)
      class(base(4)), intent(in) :: dtv
      geti = dtv%i
   end function

   subroutine start()
      allocate( b1, b2 )
      b1%i = 777
      b2%i = 888
      b3%i = 999
   end subroutine

   subroutine writeIt(unit)
      integer, intent(in) :: unit
      write ( unit, n123, iostat = stat, iomsg = msg )
   end subroutine

end module

program access001akl
use m

   open (1, file = 'access001akl.1', form='formatted', access='sequential' )
   call start()
   call writeIt(1)

   call b1%seti(333)
   call b2%seti(444)
   call b3%seti(555)

   call writeIt(1)

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 1_4
   if ( size(v_list, 1) /= 0 ) error stop 2_4

   write (unit, "('i=',I4,1X)", iostat=iostat )   dtv%geti()

   iomsg = 'dtiowrite'

end subroutine
