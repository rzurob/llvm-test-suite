! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with (non-) polymorphic zero sized derived type scalar object
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
      character(0) :: c
   end type

   type, extends(base) :: child
      character(0) :: c1
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

   class(base), allocatable :: b1
   class(base), pointer     :: b2
   type(base)               :: b3
   type(base), pointer      :: b4
   type(base), allocatable  :: b5

   namelist /nml1/ b1
   namelist /nml2/ b2
   namelist /nml3/ b3
   namelist /nml4/ b4
   namelist /nml5/ b5

end module

program scalar111a
   use m

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'scalar111a.1', form='formatted', access='sequential' )
   allocate ( child :: b1 )
   allocate ( base :: b2, b4, b5 )

   read (1,NML=nml1, iostat=stat, iomsg=msg)

   if (( stat /=  1002 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   read (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  1001 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
   read (1,NML=nml3, iostat=stat, iomsg=msg)
   if (( stat /=  1001 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
   read (1,NML=nml4, iostat=stat, iomsg=msg)
   if (( stat /=  1001 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4
   read (1,NML=nml5, iostat=stat, iomsg=msg)
   if (( stat /=  1001 ) .or. ( msg /= 'dtioread' ) ) error stop 5_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 6_4
   if ( size(v_list, 1) /= 0 ) error stop 7_4

   select type (dtv)
      type is (base)
         read ( unit, * ) dtv%c
         iostat = 1001
      type is (child)
         read ( unit, * ) dtv%c, dtv%c1
         iostat = 1002
   end select

   iomsg = 'dtioread'

end subroutine