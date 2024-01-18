!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
   type data
      integer(4)   :: i
   end type

   type base
      class(data), allocatable  :: d2
   end type

end module

program input101d
   use m

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         class(data), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat = -99
   character(150) :: msg = ''

   type(base) :: b1
   namelist /nml/ b1

   allocate(b1%d2)

   open (1, file='input101d.1', form='formatted', access='sequential', blank='zero' )

   read (1, nml, iostat = stat, iomsg = msg)

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: data

   class(data), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   type(data) :: d1
   namelist /nml/ d1

   if ( iotype /= 'NAMELIST' ) error stop 3_4
   if ( size(v_list,1) /= 0 )  error stop 4_4

   read (unit, nml, iostat=iostat )

   dtv%i = d1%i
   iomsg = 'dtioread'

end subroutine

