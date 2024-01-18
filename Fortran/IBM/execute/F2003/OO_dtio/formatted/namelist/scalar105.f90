! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting, scalar objects and with internal file (Input)
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
   type base
      integer :: i = -999
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program scalar105
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   type(base)               :: b2
   type(base), allocatable  :: b3

   character(15) :: internalFile (10) = ''

   namelist /nml/ b1, b2, b3

   allocate(b1, b3)

   write ( internalFile(1), *)    "&nml"
   write ( internalFile(2), *)    "b1= 2, b2= 4,"
   write ( internalFile(3), *)    "b3= 6"
   write ( internalFile(4), *)    "/"

   read (internalFile, NML=nml, iostat=stat, iomsg=msg)

   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   if ( b1%i /= 2 ) error stop 2_4
   if ( b2%i /= 4 ) error stop 3_4
   if ( b3%i /= 6 ) error stop 4_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 5_4
   if ( size(v_list, 1) /= 0 ) error stop 6_4

   read (unit, "(I1)", iostat=iostat )      dtv%i

   iomsg = 'dtiowrite'

end subroutine
