!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: final001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting inside final subroutine
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
      character(3) :: c = 'xxx'
   contains
      final :: finalbase
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
   contains
      final :: finalchild
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

   integer :: unit = 1
   integer :: stat
   character(200) :: msg

contains

   subroutine finalbase ( dtv )
      type(base), intent(inout) :: dtv
      namelist /FB/ dtv

      write ( unit, fb, iostat = stat, iomsg = msg )

      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end subroutine

   subroutine finalchild ( dtv )
      type(child), intent(inout) :: dtv
      namelist /FC/ dtv

      write ( unit, fc, iostat = stat, iomsg = msg )

      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
   end subroutine

end module

program final001
   use m

   class(base), allocatable :: b1
   class(child), pointer    :: c2

   open (1, file = 'final001.1', form='formatted', access='sequential' )

   allocate ( b1, source = base('IBM') )
   deallocate( b1 )
   allocate ( b1, source = child('IBM',1001) )
   deallocate ( b1 )
   allocate ( c2 )
   deallocate ( c2 )


end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base,child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   select type (dtv)
      type is (base)
         write (unit, "('c=',A3)", iostat=iostat )                  dtv%c
      type is (child)
         write (unit, "('i=',I4,1X,'c=',A3)", iostat=iostat )      dtv%i, &
                                                                   dtv%c
   end select

   iomsg = 'dtiowrite'

end subroutine
