! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic/nonpoly array with type with derived type component (Output)
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

   type :: data
      integer :: i, j
   end type

   type :: base
      type(data) :: d1
   end type

   type, extends(base) :: child
      type(data) :: d2
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

   class(base), pointer :: b2(:,:)
   namelist /nml1/ b2

end module

program array001b
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1(:)
   type(child)              :: b3(2,2)
   class(child), pointer    :: b4(:,:)

   namelist /nml2/ b1, b3
   namelist /nml2/ b4

   open (1, file = 'array001b.1', form='formatted', access='stream' )
   allocate(b1(2), source = (/ child(data(1001,1002), data(1003,1004)), child(data(1005,1006), data(1007,1008)) /))
   b3 = reshape( source = (/ child(data(1009,1010), data(1011,1012)), child(data(1013,1014), data(1015,1016)),  &
                             child(data(1017,1018), data(1019,1020)), child(data(1021,1022), data(1023,1024))/) , shape =(/2,2/))
   allocate(b4(2,2), source = b3 )
   allocate(b2(2,2), source = reshape ( source = (/ b1, b1 /) , shape = (/ 2, 2 /) )  )

   write (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   select type ( m => dtv )
      class is (base)
         write (unit, *, iostat=iostat )      m%d1
      type is (child)
         write (unit, *, iostat=iostat )      m%d1
         write (unit, *, iostat=iostat )      m%d2
   end select

   iomsg = 'dtiowrite'

end subroutine
