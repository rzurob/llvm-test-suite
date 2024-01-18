!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: scalar002.f
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
!*                                        Try namelist formatting within DTIO procedure (Output)
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
      real(4), allocatable :: i
      real(4), pointer     :: j
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

program scalar002
   use m
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   type(base)               :: b3

   integer :: stat
   character(200) :: msg

   allocate(b1, b1%i, b1%j)
   allocate(b2, b2%i, b2%j)
   allocate(b3%i, b3%j)

   b1%i = 4.0
   b1%j = 8.0
   b2%i = 12.0
   b2%j = 16.0
   b3%i = 20.0
   b3%j = 24.0

   open (1, file = 'scalar002.1', form='formatted', access='sequential' )

   write (1,*, iostat=stat, iomsg=msg)    b1
   if ( ( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write (1,*, iostat=stat, iomsg=msg)    b2
   if ( ( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write (1,*, iostat=stat, iomsg=msg)    b3
   if ( ( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   real(4), allocatable :: i
   real(4), pointer     :: j

   namelist /nml1/ i, j

   allocate (i, source = dtv%i)
   j => dtv%j

   if ( iotype /= "LISTDIRECTED" ) error stop 4_4
   if ( size(v_list, 1) /= 0 )     error stop 5_4

   write (unit, nml1, iostat=iostat )

   iomsg = 'dtiowrite'

end subroutine
