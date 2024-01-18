! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        rename: local-name is a namelist-group-name
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type base
      character(3) :: c
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), allocatable :: b1
   class(base), pointer     :: b2
   type(base)               :: b3 = base('ibm')

   namelist /nml1/ b1, b2
   namelist /nml2/ b3, b2, b1

end module


program rename002
   use m1, n1 => nml1, n2 => nml2, b11 => b1, b12 => b2

   integer :: stat
   character(200) :: msg

   open (unit = 3, file ='rename002.1', form='formatted', access='sequential')

   ! allocation of variables

   allocate (b11, source = base('abc') )
   allocate (b12, source = base('def') )

   ! unformatted I/O operations

   write ( 3, n1, iostat = stat, iomsg = msg )
   if ( (stat /= 0 ) .or. ( msg /= 'dtiowrite') )            error stop 1_4
   write ( 3, n2, iostat = stat, iomsg = msg )
   if ( (stat /= 0 ) .or. ( msg /= 'dtiowrite') )            error stop 2_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m1, only: base

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   write (unit, "('c= ', A3, 1X)", iostat=iostat )        dtv%c

   iomsg = 'dtiowrite'

end subroutine
