! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic entities contains polymorphic components (Output)
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

   type basedata
      integer(4) :: i1(2) = (/ -9,-9 /)
   end type

   type, extends(basedata) :: childdata
      integer(4) :: i2(2) = (/ -9,-9 /)
   end type

   type base
      class(basedata), pointer :: bd
   end type

   type, extends(base) :: child
      class(basedata), allocatable :: cd
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

program scalar110
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   namelist /nml/ b1, b2

   open (1, file = 'scalar110.1', form='formatted', access='sequential' )
   allocate(b1, b1%bd)
   allocate(child :: b2 )

   select type ( b2 )
      type is (child)
         allocate(childdata :: b2%bd, b2%cd)
   end select

   read (1,NML=nml, iostat=stat, iomsg=msg)

   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   if ( ( b1%bd%i1(1) /= 567 )  .or. ( b1%bd%i1(2) /= 890 ) ) error stop 2_4

   select type(b2)
      type is (child)
         select type (g => b2%bd)
            type is (childdata)
               if (( g%i1(1) /= 123 ) .or. ( g%i1(2) /= 234 ) ) error stop 3_4
               if (( g%i2(1) /= 345 ) .or. ( g%i2(2) /= 456 ) ) error stop 4_4
         end select
         select type (g=>b2%cd)
            type is (childdata)
               if (( g%i1(1) /= 567 ) .or. ( g%i1(2) /= 678 ) ) error stop 5_4
               if (( g%i2(1) /= 789 ) .or. ( g%i2(2) /= 890 ) ) error stop 6_4
         end select
   end select

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child, basedata

   interface read(formatted)
      subroutine datareadformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import basedata
         class(basedata), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   class(basedata), allocatable :: dummy1, dummy2

   namelist /basedtio/ dummy1
   namelist /childdtio/ dummy1, dummy2

   if ( iotype /= "NAMELIST" ) error stop 7_4
   if ( size(v_list, 1) /= 0 ) error stop 8_4

   select type (dtv)
      type is (child)
         allocate (dummy1, source = dtv%bd)
         allocate (dummy2, source = dtv%cd)
         read (unit, childdtio, iostat = iostat, iomsg = iomsg)
         deallocate ( dtv%bd, dtv%cd )
         allocate (dtv%bd, source = dummy1)
         allocate (dtv%cd, source = dummy2)
      class default            !<- if it's not type child, it's type base
         allocate (dummy1, source = dtv%bd)
         read (unit, basedtio, iostat = iostat, iomsg = iomsg)
         deallocate ( dtv%bd )
         allocate (dtv%bd, source = dummy1)
   end select

   if ( iomsg /= 'datadtio' ) error stop 9_4

   iomsg = 'dtioread'

end subroutine

subroutine datareadformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: basedata, childdata

   class(basedata), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 10_4
   if ( size(v_list, 1) /= 0 ) error stop 11_4

   select type (dtv)
      type is (basedata)
         read (unit, "(I4, i3)" , iostat = iostat )   dtv%i1
      type is (childdata)
         read (unit, "(2(I4))" , iostat = iostat )   dtv%i1
         read (unit, "(I4, i3)" , iostat = iostat )   dtv%i2
   end select

   iomsg = 'datadtio'

end subroutine
