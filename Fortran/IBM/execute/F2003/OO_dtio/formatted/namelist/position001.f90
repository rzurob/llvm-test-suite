! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try position edit descriptors (T, TL, TR, X) inside DTIO (Output)
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
      integer(4)   :: id
      character(3) :: name
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

end module

program position001
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   class(base), pointer     :: b2

   namelist /nml/ b1, b2

   allocate( b1, source = base(1,'ibm'))
   allocate( b2, source = base(2,'ftn'))
   open (1, file = 'position001.1', form='formatted', access='sequential' )

   write (1,NML=nml, iostat=stat, iomsg=msg)

   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   write ( unit, "(A,/)" ) "start of dtio"
   write ( unit, "(1X,'name=', 5X, 'id=', T15, I4, T8, A3,/)", iostat = iostat ) dtv%id, dtv%name
   ! write one more time in reverse order
   write ( unit, "(1X,'id=', T5, I4, TR1, 'name=', 1X, A3)", iostat = iostat ) dtv%id, dtv%name

   iomsg = 'dtiowrite'

end subroutine

